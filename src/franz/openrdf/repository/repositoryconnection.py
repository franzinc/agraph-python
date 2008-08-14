#!/usr/bin/env python
# -*- coding: utf-8 -*-

##***** BEGIN LICENSE BLOCK *****
##Version: MPL 1.1
##
##The contents of this file are subject to the Mozilla Public License Version
##1.1 (the "License") you may not use this file except in compliance with
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
from franz.openrdf.model.value import Value


# * Main interface for updating data in and performing queries on a Sesame
# * repository. By default, a RepositoryConnection is in autoCommit mode, meaning
# * that each operation corresponds to a single transaction on the underlying
# * store. autoCommit can be switched off in which case it is up to the user to
# * handle transaction commit/rollback. Note that care should be taking to always
# * properly close a RepositoryConnection after one is finished with it, to free
# * up resources and avoid unnecessary locks.
# * <p>
# * Several methods take a vararg argument that optionally specifies a (set of)
# * context(s) on which the method should operate. Note that a vararg parameter
# * is optional, it can be completely left out of the method call, in which case
# * a method either operates on a provided statements context (if one of the
# * method parameters is a statement or collection of statements), or operates on
# * the repository as a whole, completely ignoring context. A vararg argument may
# * also be 'null' (cast to Resource) meaning that the method operates on those
# * statements which have no associated context only.
# * <p>
# * Examples:
# * 
# * <pre>
# * // Ex 1: this method retrieves all statements that appear in either context1 or context2, or both.
# * RepositoryConnection.getStatements(null, null, null, true, context1, context2)
# * 
# * // Ex 2: this method retrieves all statements that appear in the repository (regardless of context).
# * RepositoryConnection.getStatements(null, null, null, true)
# * 
# * // Ex 3: this method retrieves all statements that have no associated context in the repository.
# * // Observe that this is not equivalent to the previous method call.
# * RepositoryConnection.getStatements(null, null, null, true, (Resource)null)
# * 
# * // Ex 4: this method adds a statement to the store. If the statement object itself has 
# * // a context (i.e. statement.getContext() != null) the statement is added to that context. Otherwise,
# * // it is added without any associated context.
# * RepositoryConnection.add(statement)
# * 
# * // Ex 5: this method adds a statement to context1 in the store. It completely ignores any
# * // context the statement itself has.
# * RepositoryConnection.add(statement, context1)
 

class RepositoryConnection(object):
    def __init__(self, repository, sail_connection):
        self.repository = repository
        self.sail_store = sail_connection
        
    def getSailConnection(self):
        """
        Return the underlying Sail connection.
        """
        return self.sail_store
    
    def rollback(self):
        return self.sail_store.rollback()
    
    def close(self):
        return self.sail_store.commit()
    
    def commit(self):
        return self.sail_store.commit()
    
    def prepareQuery(self, queryLanguage, queryString, baseURI):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.
        """
        return self.sail_store.prepareQuery(queryLanguage, queryLanguage, baseURI)
    


#    ## Gets all statements with a specific subject, predicate and/or object from
#    ## the repository. The result is optionally restricted to the specified set
#    ## of named contexts.
#    ## 
#    ## @param subj
#    ##        A Resource specifying the subject, or <tt>null</tt> for a
#    ##        wildcard.
#    ## @param pred
#    ##        A URI specifying the predicate, or <tt>null</tt> for a wildcard.
#    ## @param obj
#    ##        A Value specifying the object, or <tt>null</tt> for a wildcard.
#    ## @param contexts
#    ##        The context(s) to get the data from. Note that this parameter is a
#    ##        vararg and as such is optional. If no contexts are supplied the
#    ##        method operates on the entire repository.
#    ## @param includeInferred
#    ##        if false, no inferred statements are returned if true, inferred
#    ##        statements are returned if available. The default is true.
#    ## @return The statements matching the specified pattern. The result object
#    ##         is a @link RepositoryResult object, a lazy Iterator-like object
#    ##         containing @link Statements and optionally throwing a
#    ##         @link RepositoryException when an error when a problem occurs
#    ##         during retrieval.
    #RepositoryResult<Statement> 
    def getStatements(self, subj, pred,  obj, includeInferred, contexts=[]):
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a RepositoryResult that produces a 'Statement'
        each time that 'next' is called.
        """
        raise UnimplementedMethodException("getStatements")

    def getJDBCStatements(self, subj, pred,  obj, includeInferred, contexts=[]):
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a JDBCResultSet that enables Values, strings, etc.
        to be selectively extracted from the result, without the bulky overhead
        of the OpenRDF BindingSet protocol.
        """
        raise UnimplementedMethodException("getJDBCStatements")

#    ## Adds the supplied statement to this repository, optionally to one or more
#    ## named contexts.
#    ## 
#    ## @param st
#    ##        The statement to add.
#    ## @param contexts
#    ##        The contexts to add the statements to. Note that this parameter is
#    ##        a vararg and as such is optional. If no contexts are specified, the
#    ##        statement is added to any context specified in each statement, or
#    ##        if the statement contains no context, it is added without context.
#    ##        If one or more contexts are specified the statement is added to
#    ##        these contexts, ignoring any context information in the statement
#    ##        itself.
#    ## @throws RepositoryException
#    ##         If the statement could not be added to the repository, for example
#    ##         because the repository is not writable.
    def add(self, arg0, arg1=None, arg2=None, contexts=None, base=None, format=None):
        """
        Add a triple or a statement, or load a file into the repository.
        """
        raise UnimplementedMethodException("add")

    def addFile(self, filePath, base=None, format=None, context=None):
        """
        Load the file or file path 'filePath' into the store.  'base' optionally defines a base URI,
        'format' is RDFFormat.NTRIPLES or RDFFormat.RDFXML, and 'context' optionally specifies
        which context the triples will be loaded into.
        """
        raise UnimplementedMethodException("addFile")
    
    def addTriple(self, subject, predicate, object, contexts=None):
        """
        Add the supplied triple of values to this repository, optionally to
        one or more named contexts.
        """
        raise UnimplementedMethodException("addTriple")

    def addTriples(self, triples_or_quads, context=None):
        """
        Add the supplied triples or quads to this repository.  Each triple can
        be a list or a tuple of Values.   If 'context' is set, then 
        the first argument must contain only triples, and each is inserted into
        the designated context.
        """
        raise UnimplementedMethodException("addTriples")

    def addStatement(self, statement, contexts=None):
        """
        Add the supplied statement to the specified contexts in the repository.
        """
        raise UnimplementedMethodException("add")
      
#    ## Removes the supplied statement from the specified contexts in the
#    ## repository.
#    ## @param st
#    ##        The statement to remove.
#    ## @param contexts
#    ##        The context(s) to remove the data from. Note that this parameter is
#    ##        is optional. If no contexts are supplied the
#    ##        method operates on the contexts associated with the statement
#    ##        itself, and if no context is associated with the statement, on the
#    ##        entire repository.
#    ## @throws RepositoryException
#    ##         If the statement could not be removed from the repository, for
#    ##         example because the repository is not writable.
    def remove(self, statement, contexts=None):
        """
        Removes the supplied statement from the specified contexts in the repository.
        """
        raise UnimplementedMethodException("remove")

#    ## Returns the number of (explicit) statements that are in the specified
#    ## contexts in this repository.
#    ## 
#    ## @param contexts
#    ##        The context(s) to get the data from. Note that this parameter is a
#    ##        vararg and as such is optional. If no contexts are supplied the
#    ##        method operates on the entire repository.
#    ## @return The number of explicit statements from the specified contexts in
#    ##         this repository.
    def size(self, contexts):
        """
        Returns the number of (explicit) statements that are in the specified
        contexts in this repository.
        """
        raise UnimplementedMethodException("size")

#    ## Returns <tt>true</tt> if this repository does not contain any (explicit)
#    ## statements.
#    ## 
#    ## @return <tt>true</tt> if this repository is empty, <tt>false</tt>
#    ##         otherwise.
#    ## @throws RepositoryException
#    ##         If the repository could not be checked to be empty.
    def isEmpty(self):
        raise UnimplementedMethodException("isEmpty")
       


#    ## Removes all statements from a specific contexts in the repository.
#    ## 
#    ## @param contexts
#    ##        The context(s) to remove the data from. Note that this parameter is
#    ##        a vararg and as such is optional. If no contexts are supplied the
#    ##        method operates on the entire repository.
#    ## @throws RepositoryException
#    ##         If the statements could not be removed from the repository, for
#    ##         example because the repository is not writable.
    def clear(self, contexts=[]):
        """
        Removes all statements from a specific contexts in the repository.
        """
        raise UnimplementedMethodException("clear")
         
        
    ## Exports all statements with a specific subject, predicate and/or object
    ## from the repository, optionally from the specified contexts.
    ## 
    ## @param subj
    ##        The subject, or null if the subject doesn't matter.
    ## @param pred
    ##        The predicate, or null if the predicate doesn't matter.
    ## @param obj
    ##        The object, or null if the object doesn't matter.
    ## @param contexts
    ##        The context(s) to get the data from. Note that this parameter is a
    ##        vararg and as such is optional. If no contexts are supplied the
    ##        method operates on the entire repository.
    ## @param handler
    ##        The handler that will handle the RDF data.
    ## @param includeInferred
    ##        if false, no inferred statements are returned if true, inferred
    ##        statements are returned if available
    def exportStatements(self, subj, pred, obj, includeInferred, handler, contexts=None):
        """
        Exports all statements with a specific subject, predicate and/or object
        from the repository, optionally from the specified contexts.        
        """
        raise UnimplementedMethodException("exportStatements")

    ## Exports all explicit statements in the specified contexts to the supplied
    ## RDFHandler.
    ## 
    ## @param contexts
    ##        The context(s) to get the data from. Note that this parameter is a
    ##        vararg and as such is optional. If no contexts are supplied the
    ##        method operates on the entire repository.
    ## @param handler
    ##        The handler that will handle the RDF data.
    def export(self, handler, contexts=None):
        self.exportStatements(None, None, None, False, contexts=contexts)


    ## Gets all declared namespaces as a RepositoryResult of @link Namespace
    def getNamespaces(self):
        raise UnimplementedMethodException("getNamespaces")

    ## Gets the namespace that is associated with the specified prefix, if any.
    def getNamespace(self, prefix):
        raise UnimplementedMethodException("getNamespace")


    ## Sets the prefix for a namespace.
    def setNamespace(self, prefix, name):
        raise UnimplementedMethodException("setNamespace")

    ## Removes a namespace declaration by removing the association between a
    ## prefix and a namespace name.
    def removeNamespace(self, prefix):
        raise UnimplementedMethodException("removeNamespace")

    ## Removes all namespace declarations from the repository.
    def clearNamespaces(self):
        raise UnimplementedMethodException("clearNamespaces")

