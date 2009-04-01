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

class IllegalOptionException(object):
    pass

class IllegalArgumentException (object):
    pass

class InitializationException(object):
    pass

class UnimplementedMethodException(object):
    pass

class IllegalStateException (object):
    pass

class AllegroGraphException (object):
    pass

class IOException (object):
    pass

class InterruptedException (object):
    pass

class ConnectException(object):
    pass

class BadnessException(object):
    """
    General badness with no explanation.
    """
    pass

class JDBCException(object):
    """
    Exception during iterator over a JDBC ResultSet
    """
    pass

class RDFHandlerException(object):
    pass

class BadFormatException(object):
    pass

class ServerException(object):
    pass

class QuerySyntaxException(object):
    """
    Illegal Common Logic syntax
    """

class QueryMissingFeatureException(object):
    """
    Source language evokes a feature not supported by the execution language
    """
    

#class NiceException(object):
#    pass
#
#class RuntimeException(object):
#    pass
#
#class FakeException(object):
#    pass