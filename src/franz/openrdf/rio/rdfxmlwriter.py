# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################


from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.rdfwriter import RDFWriter


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
