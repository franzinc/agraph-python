#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License"); you may not use this file except in compliance with
##the License. You may obtain a copy of the License at
##http:##www.mozilla.org/MPL/
##
##Software distributed under the License is distributed on an "AS IS" basis,
##WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
##for the specific language governing rights and limitations under the
##License.
##
##The Original Code is the AllegroGraph Java Client interface.
##
##The Original Code was written by Franz Inc.
##Copyright (C) 2006 Franz Inc.  All Rights Reserved.
##
##***** END LICENSE BLOCK *****


from franz.openrdf.exceptions import *
from franz.openrdf.model.statement import Statement
from franz.openrdf.repository.repositoryresult import RepositoryResult

#############################################################################
##
#############################################################################

class QueryResult(object):
    """
    Super type of all query result types (TupleQueryResult, GraphQueryResult, etc.
    Evaluates as a Python iterator
    """
    pass

#############################################################################
##
#############################################################################

class GraphQueryResult(QueryResult):
    """
    A graph query result is an iterator of Statement's.  This is incredibly inefficient.    
    """
    def __init__(self, string_tuples):
        self.result = RepositoryResult(string_tuples)
    
    def __iter__(self): return self.result
    
    def close(self):
        """
        Shut down the iterator, to insure that resources are free'd up.
        """
        self.result.close()
        
    def next(self):
        """
        Return the next Statement in the answer, if there is one.  Otherwise,
        raise StopIteration exception.
        """
        return self.result.next()

class TupleQueryResult(QueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    def __init__(self, variable_names, string_tuples):
        self.variableNames = variable_names
        self.string_tuples = string_tuples
        self.cursor = 0        
        self.tuple_width = len(variable_names)
        self.tupleCount = len(string_tuples)        
        self.reusableRow = [None] * self.tuple_width
    
    def __iter__(self): return self
    
    def next(self):
        if self.cursor >= self.tupleCount:
            raise StopIteration()
        i = 0
        while i < self.tuple_width:
            self.reusableRow[i] = None
            i += 1
        bs = DictBindingSet(self.variableNames, self.string_tuples[self.cursor], self.reusableRow)
        self.cursor += 1
        return bs        

    def close(self):
        pass    

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.
        """
        return self.variableNames
        
    def rowCount(self): return len(self.string_tuples)

class DictBindingSet(dict):
    """
    A BindingSet is a set of named value bindings, which is used to
    represent a single query solution. Values are indexed by name of the binding
    which typically corresponds to the names of the variables used in the
    projection of the original query.
    
    DictBindingSet emulates a Sesame BindingSet, a Python dictionary and a list simultaneously.
    The internal datastructure is a pair of lists.  
    """
    def __init__(self, variableNames, string_tuple, reusable_row):
        self.variable_names = variableNames
        self.string_tuple = string_tuple
        self.reusable_row = reusable_row
    
    def _validate_index(self, index):
        if index >= 0 and index < len(self.string_tuple): return index
        else:
            raise IllegalArgumentException("Out-of-bounds index passed to BindingSet." +
                                           "  Index must be between 0 and %i, inclusive." % (len(self.string_tuple) - 1)) 
            
    def _get_ith_value(self, index):
        term = self.reusable_row[index]
        if not term:
            term = Statement.stringTermToTerm(self.string_tuple[index])
            self.reusable_row[index] = term
        return term
        
    def __getitem__(self, key):
        if isinstance(key, int): 
            #return self.string_tuple[self._validate_index(key)]
            return self._get_ith_value(self._validate_index(key))
        else:
            for i in range(len(self.variable_names)):
                if key == self.variable_names[i]:
                    return self._get_ith_value(i)
        raise IllegalArgumentException(("Illegal key '%s' passed to binding set." +
                            "\n   Legal keys are %s") % (key, str(self.variable_names)))
 
    def __len__(self):
        return len(self.reusable_row)
    
    def get(self, key):
        try:
            return self.__getitem__(key)
        except IllegalArgumentException:
            return None
        
    def addBindingPair(self, name, value):
        if value:
            self[name] = value
        
    def addBinding(self, binding):
        self.addBindingPair(binding.getName(),  binding.getValue())
        
    def removeBinding(self, name):
        try:
            del self[name]
        except:
            pass
    
    def iterator(self):
        """
        Creates an iterator over the bindings in this BindingSet. This only
        returns bindings with non-null values. An implementation is free to return
        the bindings in arbitrary order.
        
        Currently, we only support Python-style iteration over BindingSet dictionaries,
        so this (Java-style) iterator method is not implemented.
        """
        raise UnimplementedMethodException("iterator")        
    
    def getBindingNames(self):
        """
        Gets the names of the bindings in this BindingSet.

        """
        return self.iterkeys()

    def getBinding(self, bindingName):
        """
        Gets the binding with the specified name from this BindingSet.
        """
        value = self.get(bindingName)
        return Binding(bindingName, value) if value else None
        
    def hasBinding(self, bindingName):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        return self.get(bindingName) or False

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this BindingSet.
        Throws exception if 'bindingName' is not legal.
        """
        return self.__getitem__(bindingName)
    
    def getRow(self):
        """
        Return a list of strings representing the values of the current row.
        This exists mostly for debugging (otherwise, we would return terms instead of strings).
        """
        return self.string_tuple

    def size(self):
        """
        Returns the number of bindings in this BindingSet.
        """
        return len(self)
        
#    def __eq__(self, other):
#        """
#        Compares a BindingSet object to another object.
#        """
#        raise UnimplementedMethodException("__eq__")

#    def __hash__(self):
#        """
#        The hash code of a binding is defined as the bit-wise XOR of the hash
#        codes of its bindings:
#        """
#        raise UnimplementedMethodException("__hash__")
    
#    def __str__(self):
#        sb = []
#        sb.append('[')
#        isFirst = False
#        for key, value in self.iteritems:
#            if isFirst: isFirst = False
#            else: sb.append(';') 
#            sb.append(key + "=" + str(value))
#        sb.append(']');
#        return ''.join(sb)

    def __dict__(self, strings_dict=False):
        """
        Return a Python dictionary representation of this binding set.
        This exists to be called by '__str__'.
        I"M NOT SURE HOW PYTHON WILL USE IT - RMM
        """
        d = {}
        for i in range(len(self.variable_names)):
            v = self.get(i)
            if strings_dict: v = str(v)
            d[self.variable_names[i]] = v
        return d
        
    def __str__(self): return str(self.__dict__(strings_dict=True))



#############################################################################
##
#############################################################################

class Binding:
    """
    An implementation of 'Binding'
    """
    def __init__(self, name, value):
        self.name = name
        self.value = value
 
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
    
    def __str__(self): return self.name + "=" + str(self.value)
    
    def __eq__(self, other):
        """
        Compares a binding object to another object.
        """
        return isinstance(other, Binding) and self.name == other.getName() and self.value == other.getValue()

    def __hash__(self):
        """
        The hash code of a binding is defined as the bit-wise XOR of the hash
        codes of its name and value:
        """
        raise UnimplementedMethodException("__hash__")
    

#############################################################################
##
#############################################################################
