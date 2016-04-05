#!/usr/bin/env python
# -*- coding: utf-8 -*-
# pylint: disable-msg=C0103

###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import unicode_literals
from franz.miniclient.request import RequestError

class IllegalOptionException(Exception):
    pass

class IllegalArgumentException (Exception):
    pass

class RDFHandlerException(Exception):
    pass

class ServerException(Exception):
    pass

class QueryMissingFeatureException(Exception):
    """
    Source language evokes a feature not supported by the execution language
    """
