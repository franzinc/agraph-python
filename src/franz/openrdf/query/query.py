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

class QueryLanguage:
    registered_languages = []
    SPARQL = None
    def __init__(self, name):
        self.name = name
        QueryLanguage.registered_languages.append(self)
        
    def __str__(self): return self.name
    
    def getName(self): return self.name

    @staticmethod
    def values(): return QueryLanguage.registered_languages[:]
    
    @staticmethod
    def valueOf(name): 
        for ql in QueryLanguage.registered_languages:
            if ql.name.lower() == name.lower():
                return ql
        return None
    
QueryLanguage.SPARQL = QueryLanguage('SPARQL')

#############################################################################
##
#############################################################################

class Query(object):
    """
    A query on a {@link Repository} that can be formulated in one of the
    supported query languages (for example SeRQL or SPARQL). It allows one to
    predefine bindings in the query to be able to reuse the same query with
    different bindings.
    """

    def setBinding(self, name, value):
        """
        Binds the specified variable to the supplied value. Any value that was
        previously bound to the specified value will be overwritten.
        """
        raise UnimplementedMethodException("setBinding")

    def removeBinding(self, name):
        """ 
        Removes a previously set binding on the supplied variable. Calling this
        method with an unbound variable name has no effect.
        """ 
        raise UnimplementedMethodException("removeBinding")

    def getBindings(self):
        """
        Retrieves the bindings that have been set on this query. 
        """ 
        raise UnimplementedMethodException("getBindings")

    def setDataset(self, dataset):
        """
        Specifies the dataset against which to evaluate a query, overriding any
        dataset that is specified in the query itself. 
        """ 
        raise UnimplementedMethodException("setDataset")
     
    def getDataset(self):
        """
        Gets the dataset that has been set using {@link #setDataset(Dataset)}, if  any. 
        """ 
        raise UnimplementedMethodException("setBinding")
     
    def setIncludeInferred(self, getDataset):
        """
        Determine whether evaluation results of this query should include inferred
        statements (if any inferred statements are present in the repository). The
        default setting is 'true'. 
        """ 
        raise UnimplementedMethodException("setIncludeInferred")

    def getIncludeInferred(self):
        """
        Returns whether or not this query will return inferred statements (if any
        are present in the repository). 
        """ 
        raise UnimplementedMethodException("getIncludeInferred")
    

#############################################################################
##
#############################################################################

class TupleQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a tuple of values
        (resources and literals) corresponding to the variables
        or expressions in a 'select' clause (or its equivalent).
        """
        raise UnimplementedMethodException("evaluate")

class GraphQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        an iterator that produces for each step a Statement.
        """
        raise UnimplementedMethodException("evaluate")

class BooleanQuery(Query):
    
    def evaluate(self):
        """
        Execute the embedded query against the RDF store.  Return
        true or false
        """
        raise UnimplementedMethodException("evaluate")


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
    A representation of a query result as a sequence of {@link Statement}
    objects. Each query result consists of zero or more Statements and
    additionally carries information about relevant namespace declarations. Note:
    take care to always close a GraphQueryResult after use to free any resources
    it keeps hold of.
    """
    
    def getNamespaces(self):
        """
        Retrieve relevant namespaces from the query result.
        """
        raise UnimplementedMethodException("getNamespaces")

class TupleQueryResult(QueryResult):
    """
    A representation of a variable-binding query result as a sequence of
    BindingSet objects. Each query result consists of zero or more
    solutions, each of which represents a single query solution as a set of
    bindings. Note: take care to always close a TupleQueryResult after use to
    free any resources it keeps hold of.
    """
    

    def getBindingNames(self):
        """
        Get the names of the bindings, in order of projection.
        """
        raise UnimplementedMethodException("getBindingNames")
    
#############################################################################
##
#############################################################################

class BindingSet:
    """
    A BindingSet is a set of named value bindings, which is used a.o. to
    represent a single query solution. Values are indexed by name of the binding
    which typically corresponds to the names of the variables used in the
    projection of the orginal query.
    
    That said, my contention is that a Python programmer would never create
    this style of binding set object.  Instead, the natural datastructure
    is a dictionary.  Hence, in this implementation, in place of a BindingSet
    we return an object of type  DictBindingSet which is a subclass of 'dict'.
    """

    def iterator(self):
        """
        Creates an iterator over the bindings in this BindingSet. This only
        returns bindings with non-null values. An implementation is free to return
        the bindings in arbitrary order.
        """
        raise UnimplementedMethodException("iterator")
    
    def getBindingNames(self):
        """
        Gets the names of the bindings in this BindingSet.
        """
        raise UnimplementedMethodException("getBindingNames")

    def getBinding(self):
        """
        Gets the binding with the specified name from this BindingSet.
        """
        raise UnimplementedMethodException("getBinding")
        
    def hasBinding(self):
        """
        Checks whether this BindingSet has a binding with the specified name.
        """
        raise UnimplementedMethodException("hasBinding")

    def getValue(self, bindingName):
        """
        Gets the value of the binding with the specified name from this
        BindingSet.
        """
        raise UnimplementedMethodException("getValue")

    def size(self):
        """
        Returns the number of bindings in this BindingSet.
        """
        raise UnimplementedMethodException("size")
        
    def __eq__(self, other):
        """
        Compares a BindingSet object to another object.
        """
        raise UnimplementedMethodException("__eq__")

    def __hash__(self):
        """
        The hash code of a binding is defined as the bit-wise XOR of the hash
        codes of its bindings:
        """
        raise UnimplementedMethodException("__hash__")

    

#############################################################################
##
#############################################################################

class Binding:
    """
    A named value binding.
    """
 
    def getName(self):
        """
        Gets the name of the binding (e.g. the variable name).
        """
        raise UnimplementedMethodException("getName")   

    def getValue(self):
        """
        Gets the value of the binding. The returned value is never equal to
        None, such a "binding" is considered to be unbound.
        """
        raise UnimplementedMethodException("getValue")
    
    def __eq__(self, other):
        """
        Compares a binding object to another object.
        """
        raise UnimplementedMethodException("__eq__")

    def __hash__(self):
        """
        The hash code of a binding is defined as the bit-wise XOR of the hash
        codes of its name and value:
        """
        raise UnimplementedMethodException("__hash__")


#############################################################################
##
#############################################################################


class DataSet:
    """ 
    Represents a dataset against which queries can be evaluated. A dataset
    consists of a default graph, which is the <a
    href="http://www.w3.org/TR/rdf-mt/#defmerge">RDF merge</a> of one or more
    graphs, and a set of named graphs. See <a
    href="http://www.w3.org/TR/rdf-sparql-query/#rdfDataset">SPARQL Query
    Language for RDF</a> for more info.
    """
    
    def getDefaultGraphs(self):
        """
        Gets the default graph URIs of this dataset. An empty set indicates that
        the default graph is an empty graph.
        """
        raise UnimplementedMethodException("getDefaultGraphs")
    
    def getNamedGraphs(self):
        """
        Gets the named graph URIs of this dataset. An empty set indicates that
        there are no named graphs in this dataset.
        """
        raise UnimplementedMethodException("getNamedGraphs")

