#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from __future__ import absolute_import
from __future__ import unicode_literals
from builtins import object

from .rdfformat import RDFFormat

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
