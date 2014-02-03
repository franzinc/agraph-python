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
from .converter import statement2ntriples

class RDFWriter(object):
    def __init__(self, rdfFormat, filePath=None):
        self.rdf_format = rdfFormat
        self.file_path = filePath
        
    def getRDFFormat(self):
        return self.rdf_format
    
    def getFilePath(self):
        return self.file_path 

            
class NTriplesWriter(RDFWriter):
    """
    Records the format as
    NTriples, and records the 'filePath' where the serialized RDF will
    be output to.  If 'filePath' is None, output is to standard output.
    
    TODO: THERE IS A WRITER PROTOCOL IMPLEMENTED IN RDFXMLWriter THAT ISN'T
    IMPLEMENTED HERE.  CONSIDER ADDING IT (OR NOT).
    """
    def __init__(self, filePath=None):
        super(NTriplesWriter, self).__init__(RDFFormat.NTRIPLES, filePath)
        
    def handleNamespace(self, prefix, name):
        """
        NTriples doesn't use prefixes.
        """
        pass
        
    def export(self, statements):
        """
        Iterator over the statements in 'statements', and write an NTriples 
        representation of each statement to a file, or to standard output.
        """
        if self.file_path:
            file = open(self.file_path, 'w')
            strings = []
            for s in statements:
                strings.append(statement2ntriples(s, None))
            file.writelines(strings)
        else:
            buffer = []
            for s in statements:
                statement2ntriples(s, buffer)
            result = ''.join(buffer)
            print result
            
        
