# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

from franz.miniclient.request import RequestError


class IllegalOptionException(Exception):
    pass


class IllegalArgumentException(Exception):
    pass


class RDFHandlerException(Exception):
    pass


class ServerException(Exception):
    pass


class QueryMissingFeatureException(Exception):
    """
    Source language evokes a feature not supported by the execution language
    """
