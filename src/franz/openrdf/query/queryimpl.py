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
from franz.openrdf.query.query import *
from franz.openrdf.sail.repositoryresultimpl import RepositoryResultImpl

#############################################################################
##
#############################################################################

class AbstractQuery(Query):
    """
    Abstract super class of all query types
    """
    def __init__(self, queryLanguage, queryString, baseURI=None):
        self.queryLanguage = queryLanguage
        self.queryString = queryString
        self.baseURI = baseURI
        self.dataset = None
        self.includeInferred = False

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.
        """
        self.bindings.addBinding(name, value)

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.
        """ 
        self.bindings.removeBinding(name)

    def getBindings(self):
        """
        Retrieves the bindings that have been set on this query. 
        """ 
        return self.bindings

    def setDataset(self, dataset):
        """
        Specifies the dataset against which to evaluate a query, overriding any
        dataset that is specified in the query itself. 
        """ 
        self.dataset = dataset
     
    def getDataset(self):
        """
        Gets the dataset that has been set using {@link #setDataset(Dataset)}, if  any. 
        """ 
        return self.dataset
     
    def setIncludeInferred(self, includeInferred):
        """
        Determine whether evaluation results of this query should include inferred
        statements (if any inferred statements are present in the repository). The
        default setting is 'true'. 
        """ 
        self.includeInferred = includeInferred

    def getIncludeInferred(self):
        """
        Returns whether or not this query will return inferred statements (if any
        are present in the repository). 
        """ 
        return self.includeInferred

class BooleanQueryImpl(GraphQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(GraphQuery, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
    
    ## NOT YET IMPLEMENTED:
#    def evaluate(self):
#        """
#        Execute the embedded query against the RDF store.  Return
#        an iterator that produces for each step a Statement
#        """
#        statementIt = self.direct_caller.twinqlFind(self.queryString, None, 0, 0, includeInferred=self.includeInferred, more=[])
#        return statementIt


class GraphQueryImpl(GraphQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(GraphQuery, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a Statement
        """
        more = []
        statementIt = self.direct_caller.twinqlFind(self.queryString, 0, 0, includeInferred=self.includeInferred, more=more)
        return statementIt

class GraphQueryResultImpl(RepositoryResultImpl, GraphQueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    ## Method logic 100% inherited from 'RepositoryResultImpl', except for the method
    ## 'getNamespaces', which is not yet implemented.


class TupleQueryImpl(TupleQuery, AbstractQuery):
    def __init__(self, queryLanguage, queryString, baseURI=None):
        if not queryLanguage == QueryLanguage.SPARQL:
            raise IllegalOptionException("Can't evaluate the query language '%s'.  Options are: SPARQL."
                                         % QueryLanguage.SPARQL)
        super(TupleQueryImpl, self).__init__(queryLanguage, queryString, baseURI=baseURI)
        self.direct_caller = None
        
    def setDirectCaller(self, caller):
        """
        Internal call to embed the evaluator context into the query.
        """
        self.direct_caller = caller
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step at uple of values
        (resources and literals) corresponding to the variables
        or expressions in a 'select' clause (or its equivalent).
        """
        ##twinqlSelect(self, query, vars, limit, offset, useInference, moreArgs):
        bindingsIt = self.direct_caller.twinqlSelect(self.queryString, None, 0, 0, self.includeInferred, [])
        ## TODO: REWRITE TO USE AN OPENRDF BINDINGS THINGEE:
        return bindingsIt

class TupleQueryResultImpl(TupleQueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    def __init__(self, upis, types, labels, mods, more, tupleWidth, token, plimit, variableNames):
        self.upis = upis
        self.types = types,
        self.labels = labels,
        self.mods = mods
        self.tupleWidth = tupleWidth
        self.variableNames = variableNames
        self.totalBindingsCount = len(upis)
        self.tupleCount = self.totalBindingsCount / tupleWidth
        self.reusableRow = [None for k in range(tupleWidth)]
        self.socket_cursor = 0
        self.term2InternalMgr = None
    
    def __iter__(self): return self
    
    def next(self):
        if self.socket_cursor >= self.totalBindingsCount:
            raise StopIteration()
        for i in range(self.tupleWidth):
            offset = self.socket_cursor
            term = self.term2InternalMgr.assembleOpenRDFValueTerm(self.upis[offset], 
                                    self.types[0][offset], self.labels[0][offset], self.mods[offset])
            self.reusableRow[i] = term
            self.socket_cursor += 1
        return DictBindingSet(self.variableNames, self.reusableRow)
    
    def setTerm2InternalManager(self, term2InternalMgr):
        self.term2InternalMgr = term2InternalMgr

    def close(self):
        pass    

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.
        """
        return self.variableNames
    

class DictBindingSet(dict):
    """
    A BindingSet is a set of named value bindings, which is used a.o. to
    represent a single query solution. Values are indexed by name of the binding
    which typically corresponds to the names of the variables used in the
    projection of the original query.
    """
    def __init__(self, variableNames, values):
        for i in range(len(values)):
            self.addBindingPair(variableNames[i], values[i])

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
        TODO: RIGHT NOW THIS DOESN'T PRODUCE BINDING OBJECTS.  NEEDS TO BE REWRITTEN TO DO SO
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
        return BindingImpl(bindingName, value) if value else None
        
    def hasBinding(self, bindingName):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        return self.get(bindingName) or False

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this BindingSet.
        """
        return self.get(bindingName)

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
    
    def __to_string(self):
        sb = []
        sb.append('[')
        isFirst = False
        for key, value in self.iteritems:
            if isFirst: isFirst = False
            else: sb.append(';') 
            sb.append(key + "=" + str(value))
        sb.append(']');
        return ''.join(sb)


#############################################################################
##
#############################################################################

class BindingImpl:
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
