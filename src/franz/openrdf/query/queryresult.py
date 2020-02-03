################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from __future__ import unicode_literals
from builtins import range
from builtins import object

from future.utils import python_2_unicode_compatible
from past.builtins import unicode

from ..model import Statement, parse_term
from .repositoryresult import RepositoryResult

from collections import namedtuple


try:
    import franz.openrdf.query.pandas_support as pandas
    has_pandas = True
except ImportError:
    has_pandas = False


class QueryResult(object):
    """
    Super type of all query result types (TupleQueryResult, GraphQueryResult, etc.
    Evaluates as a Python iterator
    """
    def close(self):
        """
        Release resources used by this query result.
        """
        pass

    def __next__(self):
        """
        Return the next result item if there is one.

        :return: The next item.
        :raises StopIteration: If there are no more items to return.
        """
        raise NotImplemented('__next__')

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        del exc_type, exc_val, exc_tb
        self.close()

    def __del__(self):
        self.close()


#############################################################################
##
#############################################################################

class GraphQueryResult(RepositoryResult, QueryResult):
    """
    A graph query result is an iterator over the Statements.
    """
    def __init__(self, string_tuples):
        QueryResult.__init__(self)
        RepositoryResult.__init__(self, string_tuples)


class TupleQueryResult(QueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    def __init__(self, variable_names, string_tuples, metadata=None):
        QueryResult.__init__(self)
        if not isinstance(variable_names, list):
            variable_names = [variable_names]
            string_tuples = [string_tuples]
        self.variable_names = variable_names
        self.string_tuples = string_tuples
        self.cursor = 0
        self.tuple_count = len(string_tuples)
        self.binding_set = ListBindingSet(self.variable_names)
        self.metadata = metadata

    def __iter__(self):
        return self

    def __next__(self):
        """
        Return the next result tuple if there is one.

        :return: The next tuple.
        :rtype: ListBindingSet
        :raises StopIteration: If there are no more items to return.
        """
        if self.cursor >= self.tuple_count:
            raise StopIteration()

        bset = self.binding_set
        bset._reset(self.string_tuples[self.cursor])
        self.cursor += 1
        return bset

    def close(self):
        pass

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.

        :return: A list of names.
        :rtype: list[string]
        """
        return self.variable_names

    def getMetadata(self):
        """
        Get a nested dictionary containing query result metadata.

        :return: A dictionary
        :rtype: dict
        """
        return self.metadata

    def __len__(self):
        return self.tuple_count

    def rowCount(self):
        return len(self)

    def toPandas(self):
        if not has_pandas:
            raise Exception('Pandas not installed.')
        return pandas.rows_to_pandas(self, self.variable_names)


@python_2_unicode_compatible
class ListBindingSet(object):
    """
    A BindingSet is a set of named value bindings, which is used to
    represent a single query solution. Values are indexed by name of
    the binding which typically corresponds to the names of the
    variables used in the projection of the original query.

    ListBindingSet emulates an RDF4J BindingSet, a Python dictionary
    and a list simultaneously.  The internal datastructure is a pair
    of lists.
    """
    def __init__(self, variable_names):
        self.variable_names = variable_names
        self.string_tuple = None
        self.value_cache = [None] * len(variable_names)

    def _reset(self, string_tuple):
        self.string_tuple = string_tuple
        value_cache = self.value_cache
        for i in range(len(self.variable_names)):
            value_cache[i] = None

    def _validate_index(self, index):
        if index >= 0 and index < len(self.string_tuple):
            return index

        raise IndexError("Out-of-bounds index passed to BindingSet." +
                         "  Index must be between 0 and %i, inclusive." % (len(self.string_tuple) - 1))

    def _get_ith_value(self, index):
        term = self.value_cache[index]
        if not term:
            def convert(x):
                if isinstance(x, list): return [convert(elt) for elt in x]
                else: return parse_term(x)
            term = convert(self.string_tuple[index])
            self.value_cache[index] = term
        return term

    def __getitem__(self, key):
        if isinstance(key, int):
            return self._get_ith_value(self._validate_index(key))

        try:
            return self._get_ith_value(self.variable_names.index(key))
        except ValueError:
            raise KeyError(("Illegal key '%s' passed to binding set." +
                            "\n   Legal keys are %s") % (key, unicode(self.variable_names)))


    def iterator(self):
        """
        Creates an iterator over the bindings in this BindingSet. This only
        returns bindings with non-null values. An implementation is free to return
        the bindings in arbitrary order.

        Currently, we only support Python-style iteration over BindingSet dictionaries,
        so this (Java-style) iterator method is not implemented.
        """
        raise NotImplementedError("iterator")

    def getBindingNames(self):
        """
        Gets the names of the bindings in this BindingSet.

        """
        return self.variable_names

    def getBinding(self, bindingName):
        """
        Gets the binding with the specified name from this BindingSet.
        """
        return Binding(bindingName, self[bindingName])

    def hasBinding(self, bindingName):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        return self.variable_names.find(bindingName) >= 0

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this BindingSet.
        Throws exception if 'bindingName' is not legal.
        """
        return self[bindingName]

    def getRow(self):
        """
        Return a list of strings representing the values of the current row.
        This exists mostly for debugging (otherwise, we would return terms instead of strings).
        """
        return self.string_tuple

    def __len__(self):
        return len(self.value_cache)

    def size(self):
        """
        Returns the number of bindings in this BindingSet.
        """
        return len(self)

    def _toDict(self, strings_dict=False):
        """
        Return a Python dictionary representation of this binding set.
        """
        d = {}
        for i in range(len(self.variable_names)):
            v = self[i]
            if strings_dict: v = unicode(v)
            d[self.variable_names[i]] = v
        return d

    def __str__(self):
        return unicode(self._toDict(strings_dict=True))


#############################################################################
##
#############################################################################

class Binding(namedtuple('Binding', 'name value')):
    __slots__ = ()
    """
    An implementation of 'Binding'
    """
    def getName(self):
        """
        Gets the name of the binding (e.g. the variable name).
        """
        return self.name

    def getValue(self):
        """
        Gets the value of the binding. The returned value is never equal to
        None, such a "binding" is considered to be unbound.
        """
        return self.value


#############################################################################
##
