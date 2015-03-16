#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2015 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################


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


