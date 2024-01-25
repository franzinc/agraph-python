# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################


from franz.openrdf.model.utils import parse_term


class Statement:
    """
    Wraps a triple or a quad. Might also contain an id.
    """

    __slots__ = ("subject", "predicate", "object", "context", "id", "_hash")

    def __init__(self, subject, predicate, object, context=None, id=None):
        """
        Create a statement.

        Each component can be either a Value object or a string in N-Triples format.
        Strings will be parsed lazily the first time a component is accessed.
        Accessors will always return Value objects.

        :param subject: Subject component - URI, blank node, QuotedTriple or a string
                        in N-Triples format.
        :type subject: URI|BNode|str
        :param predicate: Predicate component. Either a URI, QuotedTriple or a string
                          in N-Triples format.
        :type predicate: URI|str
        :param object: Subject component. Either a Value or a string
                       in N-Triples format.
        :type object: Value|str
        :param context: Graph component (optional). Can be a URI or
                        a string in N-Triples format.
        :type context: URI|str
        :param id: Statement id (optional).
        :type id: int|str
        """
        self.subject = subject
        self.predicate = predicate
        self.object = object
        self.context = context
        self.id = id
        self._hash = None

    def __eq__(self, other):
        if not isinstance(other, Statement):
            return NotImplemented

        # The object is potentially the cheapest to check, as types
        # of these references might be different.
        # In general the number of different predicates in sets of
        # statements is the smallest, so predicate equality is checked
        # last.
        if (
            self.getObject() == other.getObject()
            and self.getSubject() == other.getSubject()
            and self.getPredicate() == other.getPredicate()
        ):
            if self.context:
                return self.getContext() == other.getContext()
            else:
                return not other.getContext()
        else:
            return False

    def __hash__(self):
        if self._hash is None:
            self._hash = hash(
                (
                    self.getSubject(),
                    self.getPredicate(),
                    self.getObject(),
                    self.getContext(),
                )
            )
        return self._hash

    def __str__(self):
        elements = [
            self.getSubject(),
            self.getPredicate(),
            self.getObject(),
            self.getContext(),
            self.getTripleID(),
        ]
        while len(elements) > 3 and elements[-1] is None:
            elements.pop()
        return "(" + ", ".join(map(str, elements)) + ")"

    def __len__(self):
        return 3 if self.context is None else 4

    def __getitem__(self, index):
        if index == 0:
            return self.getSubject()
        elif index == 1:
            return self.getPredicate()
        elif index == 2:
            return self.getObject()
        elif index == 3:
            return self.getContext()
        else:
            raise IndexError("Illegal index (%d), must be < 4" % index)

    def getSubject(self):
        """
        Get the subject (the first element of the statement).

        :return: Subject.
        :rtype: Value
        """
        # Lazily parse and replace the value if needed.
        if isinstance(self.subject, (str, bytes)):
            self.subject = parse_term(self.subject)
        return self.subject

    def setSubject(self, subject):
        self.subject = subject

    def getPredicate(self):
        """
        Get the predicate (the second element of the statement).

        :return: Predicate.
        :rtype: URI
        """
        # Lazily parse and replace the value if needed.
        if isinstance(self.predicate, (str, bytes)):
            self.predicate = parse_term(self.predicate)
        return self.predicate

    def setPredicate(self, predicate):
        self.predicate = predicate

    def getObject(self):
        """
        Get the object (the third element of the statement).

        :return: Object.
        :rtype: Value
        """
        # Lazily parse and replace the value if needed.
        if isinstance(self.object, (str, bytes)):
            self.object = parse_term(self.object)
        return self.object

    def setObject(self, object):
        self.object = object

    def getContext(self):
        """
        Get the graph (the fourth, optional element of the statement).

        :return: Graph URI.
        :rtype: URI
        """
        # Lazily parse and replace the value if needed.
        if isinstance(self.context, (str, bytes)):
            self.context = parse_term(self.context)
        return self.context

    def setContext(self, context):
        self.context = context

    def getTripleID(self):
        """
        Get the statement id.

        Note that this field might not present, depending on the way in which
        the statement has been created.

        :return: A numerical id.
        :rtype: int
        """
        # Lazily parse and replace the value if needed.
        if isinstance(self.id, (str, bytes)):
            self.id = int(self.id)
        return self.id
