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

from ..model import Literal, URI, BNode

def spoc2sparqlTerm(term):
    """
    Convert a Value to a SPARQL term.  Including a trailing blank.
    """
    if isinstance(term, URI):
        return '<%s> ' % term.getURI()
    elif isinstance(term, BNode):
        ## BUG: THIS WRITES OUT BNODES WITH NO LEADING CHAR:
        return str(term) + ' '
    elif isinstance(term, Literal):
        type = term.getDatatype()
        if type:
            return '"%s"^^<%s> ' % (term.getLabel(), type.getURI()) 
        else:
            return '"%s" ' % term.getLabel()
    else:
        raise Exception("Illegal term %s of type %s where URI or Literal expected" % (term, type(term)))
    
def value2ntriples(value):
    """
    Convert a Value to a NTriples term.  Including a trailing blank.
    """
    return spoc2sparqlTerm(value)

def statement2ntriples(statement, buffer):
    """
    If buffer is a list, append the NTriples representation (a string) of a Statement/triple.
    Otherwise, return the NTriples representation (a string) of a Statement/triple.

    Note: If there is a context, it gets dropped on the floor. 
    """
    ## TODO: THIS IS VERY SLOW, BECAUSE IT GENERATES LOTS OF OBJECTS
    ## SPEED THINGS UP FOR BOTH RepositoryResultImpl AND JDBCResultSet ITERATORS
    returnAString = buffer is None
    if returnAString: buffer = []
    buffer.append(value2ntriples(statement.getSubject()))
    buffer.append(value2ntriples(statement.getPredicate()))
    buffer.append(value2ntriples(statement.getObject()))
    buffer.append('.\n')
    return ''.join(buffer) if returnAString else None

def triples2sparql(subject, predicate, object, contexts):
    """
    Helper for converting a getTriples call into a SPARQL call.
    NOT FINISHED.  SPARQL IS SOOOO AWFUL, THAT WRITING THIS EFFICIENTLY
    IS MORE TROUPLE THAN ITS WORTH  - RMM
    """
    query = """
    select %s %s %s\n where { %s %s %s . }
    """ % (subject.getLabel() if subject else '?s',
           predicate.getLabel() if predicate else '?p',
           object.getLabel() if object else '?o',
           subject.spoc2sparqlTerm() if subject else '?s',
           predicate.spoc2sparqlTerm() if predicate else '?p',
           object.spoc2sparqlTerm() if object else '?o'
           )
    return query 
    
