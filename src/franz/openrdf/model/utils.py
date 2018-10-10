################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from franz.openrdf.model.value import URI, BNode
from franz.openrdf.model.literal import Literal
from franz.openrdf.util import strings


def parse_term(string_term):
    """
    Given a string representing a term in ntriples format, return
    a URI, Literal, or BNode.
    """
    if not string_term:
        return string_term

    if string_term.startswith('<'):
        return URI(strings.uriref(string_term))
    elif string_term.startswith('"'):
        return Literal(*strings.literal(string_term))
    elif string_term.startswith('_'):
        return BNode(strings.nodeid(string_term))
    elif string_term.startswith('default-graph'):
        return None

    return Literal(string_term)
