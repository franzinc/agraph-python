#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
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


## Represents the concept of an RDF data serialization format. RDF formats are
## identified by a {@link #getName() name} and can have one or more associated
## MIME types, zero or more associated file extensions and can specify a
## (default) character encoding. Some formats are able to encode context
## information while other are not; this is indicated by the value of
class RDFFormat(object):
    RDFXML = None     ## The RDF/XML file format.
    NTRIPLES = None   ## The N-Triples file format.
    def __init__(self, formatName, mimeTypes=[], charSet="UTF-8", fileExtensions=[], 
                 supportsNamespaces=False, supportsContexts=False):
        self.name = formatName
        self.mime_types = mimeTypes
        self.char_set = charSet
        self.file_extensions = fileExtensions
        self.supports_namespaces = supportsNamespaces
        self.supportsContexts = supportsContexts

RDFFormat.RDFXML = RDFFormat("RDF/XML", mimeTypes=["application/rdf+xml", "application/xml"], 
                        fileExtensions=["rdf", "rdfs", "owl", "xml"], supportsNamespaces=True,
                        supportsContexts=False)

RDFFormat.NTRIPLES = RDFFormat("NTRIPLES", mimeTypes=["text/plain"], fileExtensions=["nt"], charSet="US-ASCII",
                     supportsNamespaces=False, supportsContexts=False)


