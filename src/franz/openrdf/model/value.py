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

import sys

import builtins

import weakref
from six import python_2_unicode_compatible
import six

from ..util import uris, strings


# Keys used to establish ordering between different types of terms
LITERAL_CMP_KEY = 1
URI_CMP_KEY = 2
BNODE_CMP_KEY = 3


@python_2_unicode_compatible
class Value(object):
    """
    Top class in the org.openrdf.model interfaces.
    """
    __slots__ = ('__weakref__',)

    def __str__(self):
        return self.toNTriples()

    def __repr__(self):
        result = self.toNTriples()
        # Return an ascii string on Python 2, otherwise pytest gets confused
        if sys.version_info < (3,):
            result = result.encode('unicode-escape')
        return result

    def get_cmp_key(self):
        """
        Return a key that will be used to compare and hash this object.
        """
        raise NotImplementedError()

    # Comparison methods rely on get_cmp_key.
    def __eq__(self, other):
        return self is other or (isinstance(other, Value)
                                 and self.get_cmp_key() == other.get_cmp_key())

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.get_cmp_key())

    def __lt__(self, other):
        return self.get_cmp_key() < other.get_cmp_key()

    def __gt__(self, other):
        return self.get_cmp_key() > other.get_cmp_key()

    def __le__(self, other):
        return not self > other

    def __ge__(self, other):
        return not self < other

    def toNTriples(self):
        """
        Return an NTriples representation of an open rdf term
        """
        raise NotImplementedError("Failed to implement 'toNTriples' on instance of type %s" % type(self).__name__)
    
    
class Resource(Value):
    __slots__ = ()
    pass


class URI(Resource):
    """
    Lightweight implementation of the class 'URI'.
    """
    __slots__ = ('_uri', '_is_canonical')

    _instances = weakref.WeakValueDictionary()

    def __new__(cls, uri=None, namespace=None, localname=None, canonical=True):
        if isinstance(uri, URI):
            if not canonical or uri._is_canonical:
                return uri
            uri = uri.uri
        elif uri is None and namespace is not None:
            uri = namespace + (localname or '')

        if uri is None:
            raise ValueError('Either URI or namespace is required.')

        if canonical:
            result = URI._instances.get(uri)
            if result is not None:
                return result

        result = super(URI, cls).__new__(cls)
        if canonical:
            URI._instances[uri] = result
        return result

    def __init__(self, uri=None, namespace=None, localname=None, canonical=False):
        if isinstance(uri, URI):
            uri = uri.uri

        if uri is None and namespace is not None:
            uri = namespace + (localname or '')

        if uri and uri[0] == '<' and uri[len(uri) - 1] == '>':
            # be kind and trim the uri:
            uri = uri[1:-1]
        self._uri = uri
        self._is_canonical = canonical

    def get_cmp_key(self):
        return URI_CMP_KEY, self.uri
    
    def getURI(self):
        """
        Return the URI (string) for 'self'.  This method is typically
        overloaded by subclasses, which may use lazy evaluation to
        retrieve the string.
        """
        return self._uri

    uri = property(getURI)
    
    def getValue(self):
        return self.getURI()

    value = property(getValue)
    
    def getLocalName(self):
        pos = uris.getLocalNameIndex(self.getURI())
        return self.uri[pos:]
    
    localname = property(getLocalName)

    def getNamespace(self):
        pos = uris.getLocalNameIndex(self.getURI())
        return self.uri[:pos]

    def split(self):
        """
        Split into a namespace + local name pair.
        """
        pos = uris.getLocalNameIndex(self.uri)
        return self.uri[:pos], self.uri[pos:]

    namespace = property(getNamespace)

    def toNTriples(self):
        """
        Return an NTriples representation of a resource, in this case, wrap
        it in angle brackets.
        """
        return strings.encode_ntriple_uri(self.uri)

    def to_json_ld_key(self):
        """ Converts to a string to be used as a JSON-LD key. """
        return self.uri

    def to_json_ld(self):
        """ Converts to an object to be used as a JSON-LD value. """
        return {"@id": self.uri}


class BNode(Resource):
    """
    A blank node.
    """
    __slots__ = ('id',)

    def __init__(self, id=None):
        """
        Create a blank node.

        :param id: Node identifier, if not supplied one will be generated.
        """
        if id is None:
            id = 'b' + hex(builtins.id(self))
        self.id = id
        
    def getId(self):
        """
        Get the identifier of this blank node.
        """
        return self.id

    getID = getId

    getValue = getID
    
    def get_cmp_key(self):
        return BNODE_CMP_KEY, self.getId()
    
    def toNTriples(self):
        return "_:%s" % self.getId()

    def to_json_ld_key(self):
        """ Converts to a string to be used as a JSON-LD key. """
        return self.toNTriples()

    def to_json_ld(self):
        """ Converts to an object to be used as a JSON-LD value. """
        return {"@id": self.toNTriples()}


@python_2_unicode_compatible
class Namespace(object):
    """
    """
    def __init__(self, prefix, name):
        self.setPrefix(prefix)
        self.setName(name)
    
    def getName(self):
        """
        Gets the name of the current namespace (i.e. it's URI).
        """
        return self.name

    def setName(self, name): self.name = name
    
    def getPrefix(self):
        """
        Gets the prefix of the current namespace.
        """
        return self.prefix

    def setPrefix(self, prefix): self.prefix = prefix
    
    def __str__(self):
        """
        Return an odd name (that's what the RDF4J code does).
        """
        return self.prefix + " :: " + self.name
