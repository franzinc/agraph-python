# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################


from franz.openrdf.model.literal import (
    CompoundLiteral,
    GeoCoordinate,
    Literal,
    RangeLiteral,
)
from franz.openrdf.model.statement import Statement
from franz.openrdf.model.value import URI, BNode, QuotedTriple, Value


class ValueFactory:
    """
    A factory for creating URIs, blank nodes, literals and statements.
    """

    BLANK_NODE_AMOUNT = 10

    def __init__(self, store):
        self.store = store
        self.unusedBNodeIds = []

    def getUnusedBNodeId(self):
        if not self.unusedBNodeIds:
            ## retrieve a set of bnode ids (they include leading '_:', which we strip off later:
            self.unusedBNodeIds = self.store.mini_repository.getBlankNodes(
                amount=ValueFactory.BLANK_NODE_AMOUNT
            )
        return self.unusedBNodeIds.pop()[2:]  ## strip off leading '_:'

    def createBNode(self, nodeID=None):
        """
        See :meth:`.RepositoryConnection.createBNode`.
        """
        if not nodeID:
            nodeID = self.getUnusedBNodeId()
        return BNode(nodeID)

    def createLiteral(self, value, datatype=None, language=None):
        """
        See :meth:`.RepositoryConnection.createLiteral`.
        """
        if isinstance(value, (tuple, list)) and len(value) == 2:
            return self.createRange(value[0], value[1])

        return Literal(value, datatype=datatype, language=language)

    def createStatement(self, subject, predicate, _object, context=None):
        """
        See :meth:`.RepositoryConnection.createStatement`.
        """
        return Statement(subject, predicate, _object, context=context)

    def createURI(self, uri=None, namespace=None, localname=None, canonical=True):
        """
        See :meth:`.RepositoryConnection.createURI`.
        """
        if namespace is not None and localname is None:
            return URI(namespace=uri, localname=namespace, canonical=canonical)
        else:
            return URI(
                uri=uri, namespace=namespace, localname=localname, canonical=canonical
            )

    def createQuotedTriple(self, subject, predicate, object):
        """
        See :meth:`.RepositoryConnection.createQuotedTriple`
        """
        return QuotedTriple(subject, predicate, object)

    #############################################################################
    ## Extension to RDF4J API
    #############################################################################

    def validateRangeConstant(self, term, predicate):
        """Validate an individual range constant"""
        datatype = term.getDatatype()
        if not datatype:
            raise Exception(
                'Illegal term in range expression "%s" needs to '
                "have a datatype." % term.getValue()
            )

    def validateCompoundLiteral(self, term, predicate):
        """
        Check to see if range boundaries are mapped.
        TODO: ADD VALIDATION FOR GEO TERMS
        """
        if isinstance(term, RangeLiteral):
            self.validateRangeConstant(term.lowerBound, predicate)
            self.validateRangeConstant(term.upperBound, predicate)
        elif isinstance(term, GeoCoordinate):
            pass

    def object_position_term_to_openrdf_term(self, term, predicate=None):
        """
        If 'term' is a string, integer, float, etc, convert it to
        a Literal term.  Otherwise, if its a Value, just pass it through.
        """
        if term is not None:
            if isinstance(term, CompoundLiteral):
                self.validateCompoundLiteral(term, predicate)
            elif not isinstance(term, Value):
                term = self.createLiteral(term)
        return term

    def createRange(self, lowerBound, upperBound):
        """
        Create a compound literal representing a range from 'lowerBound' to 'upperBound'
        """
        lowerBound = self.object_position_term_to_openrdf_term(lowerBound)
        upperBound = self.object_position_term_to_openrdf_term(upperBound)
        return RangeLiteral(lowerBound=lowerBound, upperBound=upperBound)

    def namespace(self, prefix, canonical=True):
        """
        Create an object that allows for simple creation of URIs in given namespace.
        Attribute lookups on the returned object will produce URIs with the attribute
        name as localname. Array item access and function calls will have the same
        effect.

        :param prefix: Prefix prepended to URIs created by the returned object.
        :type prefix: str
        :param canonical: If true (default) ensure that the same URI object
                          is returned each time when the same string is
                          passed as the local name.
        :type canonical: bool
        :return: An object that can be used to create URIs.
        """
        vf = self

        class Namespace(object):
            __slots__ = ()

            def __getattribute__(self, name):
                return self(name)

            def __getitem__(self, index):
                return self(index)

            def __call__(self, arg):
                return vf.createURI(
                    namespace=prefix, localname=arg, canonical=canonical
                )

        return Namespace()

    def namespaces(self, prefixes, canonical=True):
        """
        Same as :meth:`.namespace`, but allows to create URIs in multiple namespaces
        from the strings of the form '<abbrev>:<name>', where <abbrev> is one of
        the namespace abbreviations provided in the 'prefixes' dictionary, and
        <name> is the resource name.

        :param prefixes: Dictionary mapping prefix abbreviations to prefixes to
                         be prepended to URIs created by the returned object.
        :type prefixes: dict
        :param canonical: If true (default) ensure that the same URI object
                          is returned each time when the same string is
                          passed as the local name.
        :type canonical: bool
        :return: An object that can be used to create URIs.

        """
        vf = self
        pdict = prefixes.copy()

        class Namespaces(object):
            __slots__ = ()

            def __getattribute__(self, name):
                return self(name)

            def __getitem__(self, index):
                return self(index)

            def __call__(self, arg):
                components = arg.split(":")
                abbrev, name = components[0], ":".join(components[1:])
                prefix = pdict.get(abbrev)
                assert prefix is not None, "Unknown namespace abbreviation: %s" % abbrev
                return vf.createURI(
                    namespace=prefix, localname=name, canonical=canonical
                )

        return Namespaces()
