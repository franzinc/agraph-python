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


from franz.allegrograph.exceptions import AllegroGraphException, IllegalArgumentException, IllegalStateException

class ValueObject(object):
    """
    This super class implements some of the common  methods 
    defined in the org.openrdf.model interfaces.
    """

    def __init__(self):
        self.owner = None
    
    @staticmethod
    def canReference(id):
        return id > -1
    
    def compareTo (self, to):
        return self.__str__().compareTo(to.__str__())
    
