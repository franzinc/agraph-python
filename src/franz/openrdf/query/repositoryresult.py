# pylint: disable-msg=C0103

################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################


from franz.openrdf.model import Statement
from franz.openrdf.model.utils import parse_term
from franz.openrdf.model.value import QuotedTriple
from franz.openrdf.query.pandas_support import rows_to_pandas


class RepositoryResult:
    """An iterable collection of statements.

    A RepositoryResult is a result collection of objects (for example
    :class:`org.openrdf.model.Statement`,
    :class:`org.openrdf.model.Namespace`, or
    :class:`org.openrdf.model.Resource` objects) that can be iterated
    over. It keeps an open connection to the backend for lazy
    retrieval of individual results. Additionally it has some utility
    methods to fetch all results and add them to a collection.

    By default, a RepositoryResult is not necessarily a (mathematical)
    set: it may contain duplicate objects. Duplicate filtering can be
    enabled using :meth:`enableDuplicateFilter`, but this should not
    be used lightly as the filtering mechanism is potentially
    memory-intensive.

    A RepositoryResult needs to be closed using :meth:`close` after use
    to free up any resources (open connections, read locks, etc.) it
    has on the underlying repository.
    """

    def __init__(self, string_tuples, subjectFilter=None, tripleIDs=False):
        self.string_tuples = string_tuples
        self.cursor = 0
        self.nonDuplicateSet = None
        # self.limit = limit
        self.subjectFilter = subjectFilter
        self.triple_ids = tripleIDs

    def _createStatement(self, string_tuple):
        """
        Allocate a Statement and fill it in from 'string_tuple'.
        """
        return Statement(
            *[
                QuotedTriple(*[parse_term(x) for x in term])
                if isinstance(term, list)
                else term
                for term in string_tuple
            ]
        )

    def __iter__(self):
        return self

    def close(self):
        """
        Shut down the iterator to be sure the resources are freed up.

        It is safe to call this method multiple times.
        """
        pass

    def __next__(self):
        """
        Return the next Statement in the answer, if there is one.

        Otherwise raise StopIteration exception.

        :return: The next statement.
        :raises StopIteration: If there are no more statements.
        """
        if self.nonDuplicateSet is not None:
            try:
                savedNonDuplicateSet = self.nonDuplicateSet
                self.nonDuplicateSet = None
                while True:
                    stmt = next(self)
                    if not stmt in savedNonDuplicateSet:
                        savedNonDuplicateSet.add(stmt)
                        return stmt
            finally:
                self.nonDuplicateSet = savedNonDuplicateSet
        #        elif self.limit and self.cursor >= self.limit:
        #            raise StopIteration
        elif self.cursor < len(self.string_tuples):
            stringTuple = self.string_tuples[self.cursor]
            if self.triple_ids:
                stringTuple = RepositoryResult.normalize_quint(stringTuple)
            self.cursor += 1
            if self.subjectFilter and not stringTuple[0] == self.subjectFilter:
                return next(self)
            return self._createStatement(stringTuple)
        else:
            raise StopIteration

    def enableDuplicateFilter(self):
        """
        Switch on duplicate filtering while iterating over objects.

        The RepositoryResult will keep track of the previously returned objects in a set
        and on calling next() will ignore any objects that already occur in this set.

        Caution: use of this filtering mechanism is potentially memory-intensive.
        """
        self.nonDuplicateSet = set([])

    def asList(self):
        """
        Returns a list containing all objects of this RepositoryResult in
        order of iteration.

        The RepositoryResult is fully consumed and automatically closed by this operation.
        :return: List of statements.
        :rtype: list[Statement]
        """
        result = []
        self.addTo(result)
        return result

    def addTo(self, collection):
        """
        Add all objects of this RepositoryResult to the supplied collection.

        The RepositoryResult is fully consumed and automatically closed by this
        operation.

        :param collection: The collection to add the results to.
                           It can be a list or a set.
        :type collection: set|list
        """
        isList = isinstance(collection, list)
        for stmt in self:
            if isList:
                collection.append(stmt)
            else:
                collection.add(stmt)

    def __len__(self):
        return len(self.string_tuples)

    # Python-future breaks truth testing - length is not checked...
    def __bool__(self):
        return len(self) > 0

    def rowCount(self):
        """
        Get the number of statements in this result object.

        :return: The number of results in this iterator.
        """
        return len(self)

    @staticmethod
    def normalize_quint(stringTuple):
        st = stringTuple
        return (st[1], st[2], st[3], None if len(st) == 4 else st[4], str(st[0]))

    @staticmethod
    def normalize_quad(stringTuple):
        st = stringTuple
        if len(st) == 3:
            return (st[0], st[1], st[2], None)

        return st

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        del exc_type, exc_val, exc_tb
        self.close()

    def __del__(self):
        self.close()

    def toPandas(self, include_graph=True):
        return rows_to_pandas(self, ["s", "p", "o", "g"] if include_graph else False)
