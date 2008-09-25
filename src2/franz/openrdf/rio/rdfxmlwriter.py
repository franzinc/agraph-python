#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License") you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****

from franz.openrdf.exceptions import *
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.model.value import BNode, Resource
from franz.openrdf.model.literal import Literal
from franz.openrdf.rio.rdfwriter import RDFWriter
from franz.openrdf.rio import xmlutil

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
            file.writelines(self.buffer)
        else:
            result = ''.join(self.buffer)
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
        try:
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
                number = 1
                while (prefix + number) in self.namespaceTable.itervalus(): 
                    number += 1
                prefix += number
            self.namespaceTable[name] = prefix

    def handleStatement(self, st):
        if not self.writingStarted: 
            raise RDFHandlerException("Document writing has not yet been started")
        subj = st.getSubject()
        pred = st.getPredicate()
        obj = st.getObject()
        ## Verify that an XML namespace-qualified name can be created for the
        ## predicate
        predString = str(pred)
        predSplitIdx = xmlutil.findURISplitIndex(predString)
        if predSplitIdx == -1:
            raise RDFHandlerException("Unable to create XML namespace-qualified name for predicate: "
                    + predString)
        predNamespace = predString[0:predSplitIdx]
        predLocalName = predString[predSplitIdx:]
        if not self.headerWritten: 
            self.writeHeader()
        ## SUBJECT
        if not subj == self.lastWrittenSubject: ## assumes that Resource equality is working here 
            self.flushPendingStatements()
            ## Write new subject:
            self.writeStartOfStartTag(RDF.NAMESPACE, "Description")
            if isinstance(subj, BNode): 
                bNode = subj
                self.writeAttribute(RDF.NAMESPACE, "nodeID", bNode.getID())
            else:
                uri = subj
                self.writeAttribute(RDF.NAMESPACE, "about", str(uri))                
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
                uri = objRes
                self.writeAttribute(RDF.NAMESPACE, "resource", str(uri))
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
                    self.writeAttribute(RDF.NAMESPACE, "datatype", str(datatype))
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
        prefix = self.namespaceTable.get(namespace)
        if prefix == None: 
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
        prefix = self.namespaceTable.get(namespace)
        if prefix == None or len(prefix) == 0: 
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
        prefix = self.namespaceTable.get(namespace)
        if prefix == None or len(prefix) == 0:
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
    

     
        