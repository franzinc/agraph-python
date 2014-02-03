#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2009 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import absolute_import

from .rdfformat import RDFFormat
from .rdfwriter import RDFWriter
from ..exceptions import IllegalArgumentException, RDFHandlerException
from ..vocabulary import RDF
from ..model.value import BNode, Resource
from ..model import BNode, Literal
from ..rio import xmlutil

class RDFXMLWriter(RDFWriter):
    """
    An implementation of the RDFWriter interface that writes RDF documents in
    XML-serialized RDF format.
    This records the format as
    RDF/XML, and records the 'filePath' where the serialized RDF will
    be output to.  If 'filePath' is None, output is to standard output.
    """
    def __init__(self, filePath=None):
        super(RDFXMLWriter, self).__init__(RDFFormat.RDFXML, filePath)
        self.writingStarted = False
        self.headerWritten = False
        self.lastWrittenSubject = None
        self.namespaceTable = {}
        self.buffer = []
        self.headerBuffer = None   
        self.prev = None
        
    def _catch_dup(self, stmt):
        if (self.prev and
            stmt.subject == self.prev.subject and
            stmt.predicate == self.prev.predicate and
            stmt.object == self.prev.object):
            raise Exception("Found dup")
        if (self.prev and
            str(stmt.subject) == str(self.prev.subject) and
            str(stmt.predicate) == str(self.prev.predicate) and
            str(stmt.object) == str(self.prev.object)):
            raise Exception("Found dup")
        self.prev = stmt
        
    def export(self, statements):
        try:
            self.startRDF()
            for st in statements:
                self._catch_dup(st)
                self.handleStatement(st)
            self.endRDF()        
        finally: 
            statements.close()
        if self.file_path:
            file = open(self.file_path, 'w')
            file.writelines(self.headerBuffer)
            file.writelines(self.buffer)
        else:
            combined = self.headerBuffer
            combined.extend(self.buffer)
            result = ''.join(combined)
            ## write to standard output
            print result

    def startRDF(self):
        if self.writingStarted:
            raise RDFHandlerException("Document writing has already started")
        if self.namespaceTable is None:
            raise RDFHandlerException("Failed to initialize the namespaces table before exporting.")
        self.writingStarted = True
        
    def write(self, string):
        self.buffer.append(string)

    def writeHeader(self):
        """
        Write out the header to a separate buffer.  Designed to that it can be written at 
        the end of the load, just before the string buffer is written to a file.
        Side-effect: Temporarily swap out the regular buffer while generating the
        header; then replace original buffer.
        """
        try:
            saveBuf = self.buffer
            self.buffer = []
            ## This export format needs the RDF namespace to be defined, add a
            ## prefix for it if there isn't one yet.
            self.setNamespace("rdf", RDF.NAMESPACE, False)
            self.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
            self.writeStartOfStartTag(RDF.NAMESPACE, "RDF")
            for name, prefix in self.namespaceTable.iteritems():
                self.writeNewLine()
                self.writeIndent()
                self.write("xmlns")
                if len(prefix) > 0: 
                    self.write(':')
                    self.write(prefix)                
                self.write("=\"")
                self.write(xmlutil.escapeDoubleQuotedAttValue(name))
                self.write("\"")
            self.writeEndOfStartTag()
            self.writeNewLine()
        finally: 
            self.headerBuffer = self.buffer
            self.buffer = saveBuf
            self.headerWritten = True

    def endRDF(self):
        if not self.writingStarted:
            raise RDFHandlerException("Document writing has not yet started")
        try:
            if not self.headerWritten: 
                self.writeHeader()
            self.flushPendingStatements()
            self.writeEndTag(RDF.NAMESPACE, "RDF")
            #self.writer.flush()  ## reinstate if we switch to writing lines to a file instead of to string buffer        
        finally: 
            self.writingStarted = False
            self.headerWritten = False

    def handleNamespace(self, prefix, name): 
        self.setNamespace(prefix, name, False)
        
#    def _dict_has_value(self, dictionary, value):
#        for v in dictionary.itervalues():
#            if v == value
#        

    def getNamespacePrefix(self, namespace):
        """
        Return the prefix assigned to 'namespace', or None
        """
        return self.namespaceTable.get(namespace, None)
    
    def setNamespace(self, prefix, name, fixedPrefix): 
        if self.headerWritten:
            ## Header containing namespace declarations has already been written
            return
        if not name in self.namespaceTable:
            ## Namespace not yet mapped to a prefix, try to give it the specified
            ## prefix            
            isLegalPrefix = len(prefix) == 0 or xmlutil.isNCName(prefix)            
            if  not isLegalPrefix or prefix in self.namespaceTable.itervalues():
                ## Specified prefix is not legal or the prefix is already in use,
                ## generate a legal unique prefix
                if fixedPrefix:
                    if isLegalPrefix: 
                        raise IllegalArgumentException("Prefix is already in use: " + prefix)
                    
                    else:
                        raise IllegalArgumentException("Prefix is not a valid XML namespace prefix: " + prefix)
                if len(prefix) == 0 or not isLegalPrefix: 
                    prefix = "ns"
                number = 2
                while (prefix + str(number)) in self.namespaceTable.itervalues(): 
                    number += 1
                prefix += str(number)
            self.namespaceTable[name] = prefix

    def handleStatement(self, st):
        if not self.writingStarted: 
            raise RDFHandlerException("Document writing has not yet been started")
        subj = st.getSubject()
        pred = st.getPredicate()
        obj = st.getObject()
        ## Verify that an XML namespace-qualified name can be created for the
        ## predicate
        predString = pred.uri
        predSplitIdx = xmlutil.findURISplitIndex(predString)
        if predSplitIdx == -1:
            raise RDFHandlerException("Unable to create XML namespace-qualified name for predicate: "
                    + predString)
        predNamespace = predString[0:predSplitIdx]
        predLocalName = predString[predSplitIdx:]
        ## don't write the header out now; wait until the end, so that we can create namespace
        ## prefixes on the fly:
#        if not self.headerWritten: 
#            self.writeHeader()
        ## SUBJECT
        if not subj == self.lastWrittenSubject: ## assumes that Resource equality is working here 
            self.flushPendingStatements()
            ## Write new subject:
            self.writeStartOfStartTag(RDF.NAMESPACE, "Description")
            if isinstance(subj, BNode): 
                bNode = subj
                self.writeAttribute(RDF.NAMESPACE, "nodeID", bNode.getID())
            else:
                uri = subj.uri
                self.writeAttribute(RDF.NAMESPACE, "about", uri)                

            self.writeEndOfStartTag()
            self.writeNewLine()
            self.lastWrittenSubject = subj
        ## PREDICATE
        self.writeIndent()
        self.writeStartOfStartTag(predNamespace, predLocalName)
        ## OBJECT
        if isinstance(obj, Resource): 
            objRes = obj
            if isinstance(objRes, BNode): 
                bNode = objRes
                self.writeAttribute(RDF.NAMESPACE, "nodeID", bNode.getID())                
            else:
                uri = objRes.uri
                self.writeAttribute(RDF.NAMESPACE, "resource", uri)
            self.writeEndOfEmptyTag()
        elif isinstance(obj, Literal):
            objLit = obj
            ## language attribute
            if objLit.getLanguage():
                self.writeAttributeWithoutNamespace("xml:lang", objLit.getLanguage())
            ## datatype attribute
            isXMLLiteral = False
            datatype = objLit.getDatatype()
            if datatype:
                ## Check if datatype is rdf:XMLLiteral
                isXMLLiteral = (datatype == RDF.XMLLITERAL)  ## assumes that Literal equality operator is working
                if isXMLLiteral: 
                    self.writeAttribute(RDF.NAMESPACE, "parseType", "Literal")
                else: 
                    self.writeAttribute(RDF.NAMESPACE, "datatype", datatype.uri)
            self.writeEndOfStartTag()
            ## label
            if isXMLLiteral: 
                ## Write XML literal as plain XML
                self.write(objLit.getLabel())                
            else:
                self.writeCharacterData(objLit.getLabel())
            self.writeEndTag(predNamespace, predLocalName)
        self.writeNewLine()
        ## Don't write </rdf:Description> yet, maybe the next statement
        ## has the same subject.

    def handleComment(self, comment):
        if not self.headerWritten:
            self.writeHeader()
        self.flushPendingStatements()
        self.write("<!-- ")
        self.write(comment)
        self.write(" -->")
        self.writeNewLine()

    def flushPendingStatements(self):
        if self.lastWrittenSubject:
            ## The last statement still has to be closed:
            self.writeEndTag(RDF.NAMESPACE, "Description")
            self.writeNewLine()
            self.lastWrittenSubject = None
    
    def writeStartOfStartTag(self, namespace, localName):
        prefix = self.getNamespacePrefix(namespace)
        if prefix is None:
            ## generate new prefix for 'namespace':
            self.setNamespace("ns", namespace, None)
            prefix = self.getNamespacePrefix(namespace)
        if prefix is None:
            self.write("<")
            self.write(localName)
            self.write(" xmlns=\"")
            self.write(xmlutil.escapeDoubleQuotedAttValue(namespace))
            self.write("\"")        
        elif len(prefix) == 0: 
            ## default namespace
            self.write("<")
            self.write(localName)        
        else:
            self.write("<")
            self.write(prefix)
            self.write(":")
            self.write(localName)
        
    def writeAttributeWithoutNamespace(self, attName, value):
        self.write(" ")
        self.write(attName)
        self.write("=\"")
        self.write(xmlutil.escapeDoubleQuotedAttValue(value))
        self.write("\"")

    def writeAttribute(self, namespace, attName, value):
        prefix = self.getNamespacePrefix(namespace)
        if prefix is None or len(prefix) == 0: 
            raise Exception("No prefix has been declared for the namespace used in this attribute: "
                    + namespace)
        self.write(" ")
        self.write(prefix)
        self.write(":")
        self.write(attName)
        self.write("=\"")
        self.write(xmlutil.escapeDoubleQuotedAttValue(value))
        self.write("\"")

    def writeEndOfStartTag(self, ):
        self.write(">")

    def writeEndOfEmptyTag(self, ):
        self.write("/>")

    def writeEndTag(self, namespace, localName):
        prefix = self.getNamespacePrefix(namespace)
        if prefix is None or len(prefix) == 0:
            self.write("</")
            self.write(localName)
            self.write(">")        
        else:
            self.write("</")
            self.write(prefix)
            self.write(":")
            self.write(localName)
            self.write(">")

    def writeCharacterData(self, chars):
        self.write(xmlutil.escapeCharacterData(chars))

    def writeIndent(self):
        self.write("    ")

    def writeNewLine(self):
        self.write("\n")
    

     
        
