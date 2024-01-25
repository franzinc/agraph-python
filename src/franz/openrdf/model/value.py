# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

import builtins
import weakref

from franz.openrdf.exceptions import IllegalArgumentException
from franz.openrdf.util import strings, uris

# Keys used to establish ordering between different types of terms
LITERAL_CMP_KEY = 1
URI_CMP_KEY = 2
BNODE_CMP_KEY = 3
QUOTED_TRIPLE_CMP_KEY = 4


class Value:
    """
    Top class in the org.openrdf.model interfaces.
    """

    __slots__ = ("__weakref__",)

    def __str__(self):
        return self.toNTriples()

    def __repr__(self):
        result = self.toNTriples()
        return result

    def get_cmp_key(self):
        """
        Return a key that will be used to compare and hash this object.
        """
        raise NotImplementedError()

    # Comparison methods rely on get_cmp_key.
    def __eq__(self, other):
        return self is other or (
            isinstance(other, Value) and self.get_cmp_key() == other.get_cmp_key()
        )

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
        raise NotImplementedError(
            "Failed to implement 'toNTriples' on instance of type %s"
            % type(self).__name__
        )


class Resource(Value):
    __slots__ = ()
    pass


class URI(Resource):
    """
    Lightweight implementation of the class 'URI'.
    """

    __slots__ = ("_uri", "_is_canonical")

    _instances = weakref.WeakValueDictionary()

    def __new__(cls, uri=None, namespace=None, localname=None, canonical=True):
        if isinstance(uri, URI):
            if not canonical or uri._is_canonical:
                return uri
            uri = uri.uri
        elif uri is None and namespace is not None:
            uri = namespace + (localname or "")

        if uri is None:
            raise ValueError("Either URI or namespace is required.")

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
            uri = namespace + (localname or "")

        if uri and uri[0] == "<" and uri[len(uri) - 1] == ">":
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
        """Converts to a string to be used as a JSON-LD key."""
        return self.uri

    def to_json_ld(self):
        """Converts to an object to be used as a JSON-LD value."""
        return {"@id": self.uri}


class BNode(Resource):
    """
    A blank node.
    """

    __slots__ = ("id",)

    def __init__(self, id=None):
        """
        Create a blank node.

        :param id: Node identifier, if not supplied one will be generated.
        """
        if id is None:
            id = "b" + hex(builtins.id(self))
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
        """Converts to a string to be used as a JSON-LD key."""
        return self.toNTriples()

    def to_json_ld(self):
        """Converts to an object to be used as a JSON-LD value."""
        return {"@id": self.toNTriples()}


class Namespace:
    """ """

    def __init__(self, prefix, name):
        self.setPrefix(prefix)
        self.setName(name)

    def getName(self):
        """
        Gets the name of the current namespace (i.e. it's URI).
        """
        return self.name

    def setName(self, name):
        self.name = name

    def getPrefix(self):
        """
        Gets the prefix of the current namespace.
        """
        return self.prefix

    def setPrefix(self, prefix):
        self.prefix = prefix

    def __str__(self):
        """
        Return an odd name (that's what the RDF4J code does).
        """
        return self.prefix + " :: " + self.name


class QuotedTriple(Resource):
    """
    RDF-star quoted triples.
    """

    __slots__ = ("_subject", "_predicate", "_object", "_hash")

    _subject: Value
    _predicate: Value
    _object: Value

    def __init__(self, subject: Value, predicate: Value, object: Value):
        self._hash = None

        for arg in (subject, predicate, object):
            if not isinstance(arg, Value):
                raise IllegalArgumentException(
                    "expecting a Resource, a Literal, or a QuotedTriple, but got: %s"
                    % arg
                )

        self._subject = subject
        self._predicate = predicate
        self._object = object

    def get_cmp_key(self):
        return (
            QUOTED_TRIPLE_CMP_KEY,
            self.getSubject().get_cmp_key()
            + self.getPredicate().get_cmp_key()
            + self.getObject().get_cmp_key(),
        )

    def __hash__(self):
        if self._hash is None:
            self._hash = hash(
                (self.getSubject(), self.getPredicate(), self.getObject())
            )
        return self._hash

    def getSubject(self):
        return self._subject

    def getPredicate(self):
        return self._predicate

    def getObject(self):
        return self._object

    def toNTriples(self):
        return (
            "<< "
            + " ".join(
                [
                    val.toNTriples()
                    for val in (self._subject, self._predicate, self._object)
                ]
            )
            + " >>"
        )

    def __iter__(self):
        return iter([self._subject, self._predicate, self._object])
