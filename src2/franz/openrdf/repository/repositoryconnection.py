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

import os

from franz.openrdf.exceptions import *
from franz.openrdf.model.value import Value
from franz.openrdf.repository.repositoryresult import RepositoryResult
from franz.openrdf.repository.jdbcresultset import JDBCResultSet
from franz.openrdf.model.statement import Statement
from franz.openrdf.query.query import Query, TupleQuery, GraphQuery, BooleanQuery
from franz.openrdf.query.dataset import ALL_CONTEXTS, MINI_NULL_CONTEXT
from franz.openrdf.rio.rdfformat import RDFFormat

from franz.openrdf.vocabulary.rdf import RDF
from franz.openrdf.vocabulary.rdfs import RDFS
from franz.openrdf.vocabulary.owl import OWL
from franz.openrdf.vocabulary.xmlschema import XMLSchema

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
# * RepositoryConnection.getStatements(null, null, null, true, context1, context2);
# * 
# * // Ex 2: this method retrieves all statements that appear in the repository (regardless of context).
# * RepositoryConnection.getStatements(null, null, null, true);
# * 
# * // Ex 3: this method retrieves all statements that have no associated context in the repository.
# * // Observe that this is not equivalent to the previous method call.
# * RepositoryConnection.getStatements(null, null, null, true, (Resource)null);
# * 
# * // Ex 4: this method adds a statement to the store. If the statement object itself has 
# * // a context (i.e. statement.getContext() != null) the statement is added to that context. Otherwise,
# * // it is added without any associated context.
# * RepositoryConnection.add(statement);
# * 
# * // Ex 5: this method adds a statement to context1 in the store. It completely ignores any
# * // context the statement itself has.
# * RepositoryConnection.add(statement, context1);

class RepositoryConnection(object):
    def __init__(self, repository):
        self.repository = repository 
        self.mini_repository = repository.mini_repository
        self.is_closed = False
        
    def rollback(self):
        print "PRETENDING TO ROLLBACK"
    
    def close(self):
        self.is_closed = True
    
    def commit(self):
        print "PRETENDING TO COMMIT"
    
    def prepareQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.
        """
        ## THIS IS BOGUS; OR IS IT?  WE DON'T KNOW WHAT KIND OF QUERY IT IS:
        query = Query(queryString, queryLanguage, baseURI)
        query.setConnection(self)
        return query

    def prepareTupleQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Embed 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a SELECT
        query.  The result of query
        execution is an iterator of tuples.
        """
        query = TupleQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)
        return query

    def prepareGraphQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be a CONSTRUCT
        or DESCRIBE query.  The result of query
        execution is an iterator of statements/quads.
        """
        query = GraphQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)        
        return query

    def prepareBooleanQuery(self, queryLanguage, queryString, baseURI=None):
        """
        Parse 'queryString' into a query object which can be
        executed against the RDF storage.  'queryString' must be an ASK
        query.  The result is true or false.
        """
        query = BooleanQuery(queryLanguage, queryString, baseURI=baseURI)
        query.setConnection(self)
        return query



#     * Returns the number of (explicit) statements that are in the specified
#     * contexts in this repository.
#     * 
#     * @param contexts
#     *        The context(s) to get the data from. Note that this parameter is a
#     *        vararg and as such is optional. If no contexts are supplied the
#     *        method operates on the entire repository.
#     * @return The number of explicit statements from the specified contexts in
#     *         this repository.
    def size(self, contexts=ALL_CONTEXTS):
        """
        Returns the number of (explicit) statements that are in the specified
        contexts in this repository.
        """
        if contexts == ALL_CONTEXTS or contexts is None:
            return self.mini_repository.getSize()
        else:
            print "Computing the size of a context is currently very expensive"
            resultSet = self.getJDBCStatements(None, None, None, contexts)
            count = 0
            while resultSet.next():
                count += 1
            return count
                

#     * Returns <tt>true</tt> if this repository does not contain any (explicit)
#     * statements.
#     * 
#     * @return <tt>true</tt> if this repository is empty, <tt>false</tt>
#     *         otherwise.
#     * @throws RepositoryException
#     *         If the repository could not be checked to be empty.
    def isEmpty(self):
        return self.size() == 0
    
    def _context_to_ntriples(self, context, none_is_mini_null=False):
        if context: return context.toNTriples()
        elif none_is_mini_null: return MINI_NULL_CONTEXT
        else: return None            
       
    def _contexts_to_ntriple_contexts(self, contexts, none_is_mini_null=False):
        """
        Do three transformations here.  Convert from context object(s) to
        context strings (angle brackets).
        Also, convert singleton context to list of contexts, and convert
        ALL_CONTEXTS to None.
        And, convert None context to 'null'.
        """
        if contexts == ALL_CONTEXTS or contexts is None:
            ## consistency would dictate that  None => [None], but this would
            ## likely surprise users, so we don't do that:
            cxts = None
        elif contexts is None:
            if none_is_mini_null: cxts = [MINI_NULL_CONTEXT]
            else: cxts = None
        elif isinstance(contexts, (list, tuple)):
            cxts = [(c.toNTriples() if c else MINI_NULL_CONTEXT) for c in contexts]
        else:
            cxts = [contexts.toNTriples()]
        return cxts

#     * Gets all statements with a specific subject, predicate and/or object from
#     * the repository. The result is optionally restricted to the specified set
#     * of named contexts.
#     * 
#     * @param subj
#     *        A Resource specifying the subject, or <tt>null</tt> for a
#     *        wildcard.
#     * @param pred
#     *        A URI specifying the predicate, or <tt>null</tt> for a wildcard.
#     * @param obj
#     *        A Value specifying the object, or <tt>null</tt> for a wildcard.
#     * @param contexts
#     *        The context(s) to get the data from. Note that this parameter is a
#     *        vararg and as such is optional. If no contexts are supplied the
#     *        method operates on the entire repository.
#     * @param includeInferred
#     *        if false, no inferred statements are returned; if true, inferred
#     *        statements are returned if available. The default is true.
#     * @return The statements matching the specified pattern. The result object
#     *         is a {@link RepositoryResult} object, a lazy Iterator-like object
#     *         containing {@link Statement}s and optionally throwing a
#     *         {@link RepositoryException} when an error when a problem occurs
#     *         during retrieval.
    #RepositoryResult<Statement> 
    def getStatements(self, subject, predicate,  object, contexts=ALL_CONTEXTS, includeInferred=False):
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a RepositoryResult that produces a 'Statement'
        each time that 'next' is called.
        """
        object = self.repository.getValueFactory().object_position_term_to_openrdf_term(object, predicate=predicate)
        stringTuples = self.mini_repository.getStatements(self._to_ntriples(subject), self._to_ntriples(predicate),
                 self._to_ntriples(object), self._contexts_to_ntriple_contexts(contexts), infer=includeInferred)
        return RepositoryResult(stringTuples)
    
    COLUMN_NAMES = ['subject', 'predicate', 'object', 'context']
       
    def getJDBCStatements(self, subject, predicate,  object, contexts=ALL_CONTEXTS, includeInferred=False):        
        """
        Gets all statements with a specific subject, predicate and/or object from
        the repository. The result is optionally restricted to the specified set
        of named contexts.  Returns a JDBCResultSet that enables Values, strings, etc.
        to be selectively extracted from the result, without the bulky overhead
        of the OpenRDF BindingSet protocol.
        """
        object = self.repository.getValueFactory().object_position_term_to_openrdf_term(object, predicate=predicate)
        stringTuples = self.mini_repository.getStatements(self._to_ntriples(subject), self._to_ntriples(predicate),
                 self._to_ntriples(object), self._contexts_to_ntriple_contexts(contexts), infer=includeInferred)
        return JDBCResultSet(stringTuples, column_names = RepositoryConnection.COLUMN_NAMES)

    def add(self, arg0, arg1=None, arg2=None, contexts=None, base=None, format=None):
        """
        Calls addTriple, addStatement, or addFile.  If 'contexts' is not
        specified, adds to the null context.
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, (str, file)):
            if contexts:
                if len(contexts) > 1:
                    raise IllegalArgumentException("Only one context may be specified when loading from a file.")
                context = contexts[0]
            else:
                context = None
            return self.addFile(arg0, base=base, format=format, context=context)
        elif isinstance(arg0, Value):
            return self.addTriple(arg0, arg1, arg2, contexts=contexts)
        elif isinstance(arg0, Statement):
            return self.addStatement(arg0, contexts=contexts)
        elif hasattr(arg0, '__iter__'):
            for s in arg0:
                self.addStatement(s, contexts=contexts)
        else:
            raise IllegalArgumentException("Illegal first argument to 'add'.  Expected a Value, Statement, File, or string.")
            
    def addFile(self, filePath, base=None, format=None, context=None, serverSide=False):
        """
        Load the file or file path 'filePath' into the store.  'base' optionally defines a base URI,
        'format' is RDFFormat.NTRIPLES or RDFFormat.RDFXML, and 'context' optionally specifies
        which context the triples will be loaded into.
        """
        if isinstance(filePath, file):
            filePath = os.path.abspath(filePath.name)
        elif isinstance(filePath, str):
            if not filePath.startswith('/') and not filePath.lower().startswith('c:') and not filePath.lower().startswith("http:"):
                ## looks like its a relative file path; test to see if there is a local file that matches.
                ## If so, generate an absolute path name to enable AG server to read it:
                if os.path.exists(os.path.abspath(filePath)):
                    filePath = os.path.abspath(filePath)
        if isinstance(context, (list, tuple)):
            if len(context) > 1:
                raise IllegalArgumentException("Multiple contexts passed to 'addFile': %s" % context)
            context = context[0] if context else None
        contextString = self._context_to_ntriples(context, none_is_mini_null=True)
        if format == RDFFormat.NTRIPLES or filePath.lower().endswith('.nt'):
            self.mini_repository.loadFile(filePath, 'ntriples', context=contextString, serverSide=serverSide)
        elif format == RDFFormat.RDFXML or filePath.lower().endswith('.rdf') or filePath.lower().endswith('.owl'):
            self.mini_repository.loadFile(filePath, 'rdf/xml', context=contextString, baseURI=base, serverSide=serverSide)
        else:
            raise Exception("Failed to specify a format for the file '%s'." % filePath)
        
    def addTriple(self, subject, predicate, object, contexts=None):
        """
        Add the supplied triple of values to this repository, optionally to
        one or more named contexts.        
        """       
        self.mini_repository.addStatement(self._to_ntriples(subject), self._to_ntriples(predicate),
                        self._to_ntriples(object), self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True))
    
    def _to_ntriples(self, term):
        """
        If 'term' is an OpenRDF term, convert it to a string.  If its already
        a string; assume its in ntriples format, and just pass it through.
        """
        if not term: return term
        elif isinstance(term, str): return term
        else: return term.toNTriples()
        
    def addTriples(self, triples_or_quads, context=ALL_CONTEXTS):
        """
        Add the supplied triples or quads to this repository.  Each triple can
        be a list or a tuple of Values.   If 'context' is set, then 
        the first argument must contain only triples, and each is inserted into
        the designated context.
        """
        ntripleContexts = self._contexts_to_ntriple_contexts(context, none_is_mini_null=True)
        quads = []
        for q in triples_or_quads:
            quad = [None] * 4
            if isinstance(quad, (list, tuple)):
                quad[0] = self._to_ntriples(q[0])
                quad[1] = self._to_ntriples(q[1])
                quad[2] = self._to_ntriples(q[2])
                quad[3] = self._to_ntriples(q[3]) if q[3] else ntripleContexts
            else:
                quad[0] = self._to_ntriples(q.getSubject())
                quad[1] = self._to_ntriples(q.getPredicate())
                quad[2] = self._to_ntriples(q.getObject())
                quad[3] = self._to_ntriples(q.getContext()) if q.getContext() else ntripleContexts
                
#     * Adds the supplied statement to this repository, optionally to one or more
#     * named contexts.
#     * 
#     * @param st
#     *        The statement to add.
#     * @param contexts
#     *        The contexts to add the statements to. Note that this parameter is
#     *        a vararg and as such is optional. If no contexts are specified, the
#     *        statement is added to any context specified in each statement, or
#     *        if the statement contains no context, it is added without context.
#     *        If one or more contexts are specified the statement is added to
#     *        these contexts, ignoring any context information in the statement
#     *        itself.
#     * @throws RepositoryException
#     *         If the statement could not be added to the repository, for example
#     *         because the repository is not writable.


    def addStatement(self, statement, contexts=None):
        """
        Add the supplied statement to the specified contexts in the repository.
        """        
        self.addTriple(statement.getSubject(), statement.getPredicate(), statement.getObject(),
                       contexts=contexts)      

    def remove(self, arg0, arg1=None, arg2=None, contexts=None):
        """
        Remove the supplied triple of values from this repository, optionally to
        one or more named contexts.
        """
        if contexts and not isinstance(contexts, list):
            contexts = [contexts]
        if isinstance(arg0, Value) or arg0 is None: self.removeTriples(arg0, arg1, arg2, contexts=contexts)
        elif isinstance(arg0, Statement): self.removeStatement(arg0, contexts=contexts)
        elif hasattr(arg0, '__iter__'):
            for s in arg0:
                self.removeStatement(s, contexts=contexts)
        else:
            raise IllegalArgumentException("Illegal first argument to 'remove'.  Expected a Value, Statement, or iterator.")

    def removeTriples(self, subject, predicate, object, contexts=None):
        """
        Removes the statement(s) with the specified subject, predicate and object
        from the repository, optionally restricted to the specified contexts.
        """
        ## NEED TO FIGURE OUT HOW WILDCARD CONTEXT LOOKS HERE!!!
        ## THIS IS BOGUS FOR 'None' CONTEXT; COMPLETELY AMBIGUOUS:
        ntripleContexts = self._contexts_to_ntriple_contexts(contexts, none_is_mini_null=True)        
        self.mini_repository.deleteMatchingStatements(self._to_ntriples(subject),
                self._to_ntriples(predicate), self._to_ntriples(object),
                self._to_ntriples(contexts) if contexts else ntripleContexts)
   
#     * Removes the supplied statement from the specified contexts in the
#     * repository.
#     * @param st
#     *        The statement to remove.
#     * @param contexts
#     *        The context(s) to remove the data from. Note that this parameter is
#     *        is optional. If no contexts are supplied the
#     *        method operates on the contexts associated with the statement
#     *        itself, and if no context is associated with the statement, on the
#     *        entire repository.
#     * @throws RepositoryException
#     *         If the statement could not be removed from the repository, for
#     *         example because the repository is not writable.
    def removeStatement(self, statement, contexts=None):
        """
        Removes the supplied statement(s) from the specified contexts in the repository.
        """
        self.removeTriples(statement.getSubject(), statement.getPredicate(), statement.getContext(), contexts=contexts)

#     * Removes all statements from a specific contexts in the repository.
#     * 
#     * @param contexts
#     *        The context(s) to remove the data from. Note that this parameter is
#     *        a vararg and as such is optional. If no contexts are supplied the
#     *        method operates on the entire repository.
#     * @throws RepositoryException
#     *         If the statements could not be removed from the repository, for
#     *         example because the repository is not writable.
    def clear(self, contexts=None):
        """
        Removes all statements from designated contexts in the repository.  If
        'contexts' is None, clears the repository of all statements.
        """
        self.removeTriples(None, None, None, contexts=contexts)
         
    ## Exports all explicit statements in the specified contexts to the supplied
    ## RDFHandler.
    ## 
    ## @param contexts
    ##        The context(s) to get the data from. Note that this parameter is a
    ##        vararg and as such is optional. If no contexts are supplied the
    ##        method operates on the entire repository.
    ## @param handler
    ##        The handler that will handle the RDF data.
    def export(self, handler, contexts=ALL_CONTEXTS):
        self.exportStatements(None, None, None, False, handler, contexts=contexts)

    def exportStatements(self, subj, pred, obj, includeInferred, handler, contexts=ALL_CONTEXTS):
        """
        Exports all statements with a specific subject, predicate and/or object
        from the repository, optionally from the specified contexts.        
        """
        for prefix, name in self.getNamespaces().iteritems():
            handler.handleNamespace(prefix, name)
        statements = self.getStatements(subj, pred, obj, contexts, includeInferred=includeInferred)
        handler.export(statements)

    #############################################################################################
    ## Server-side implementation of namespaces
    #############################################################################################
      
#    ## Get all declared prefix/namespace pairs
#    def getNamespaces(self):
#        dict = {}
#        for pair in self.mini_repository.listNamespaces():
#            dict[pair[0]] = pair[1]
#        print "GET NAMESPACES", dict
#        return dict        
#
#    ## Gets the namespace that is associated with the specified prefix, if any.
#    def getNamespace(self, prefix):
#        return self.getNamespaces().get(prefix)
#
#    ## Sets the prefix for a namespace.
#    def setNamespace(self, prefix, name):
#        self.mini_repository.addNamespace(prefix, name)
#
#    ## Removes a namespace declaration by removing the association between a
#    ## prefix and a namespace name.
#    def removeNamespace(self, prefix):
#        self.mini_repository.deleteNamespace(prefix)
#
#    ## Removes all namespace declarations from the repository.
#    def clearNamespaces(self):
#        for prefix in self.getNamespaces().iterkeys():
#            self.removeNamespace(prefix)

    
    #############################################################################################
    ## In-memory implementation of namespaces
    #############################################################################################
    

    NAMESPACES_MAP = {}
    
    def _get_namespaces_map(self):
        map = RepositoryConnection.NAMESPACES_MAP
        if not map:
            map.update({"rdf": RDF.NAMESPACE, 
                        "rdfs": RDFS.NAMESPACE,
                        "xsd": XMLSchema.NAMESPACE,
                        "owl": OWL.NAMESPACE, 
                        "fti": "http://franz.com/ns/allegrograph/2.2/textindex/",                                             
                        })
        return map
    
    def getNamespaces(self):
        """
        Return a dictionary of prefix/namespace pairings.
        """
        return self._get_namespaces_map()

    def getNamespace(self, prefix):
        """
        Return the namespace that is associated with the specified prefix, if any.
        """
        return self._get_namespaces_map().get(prefix.lower())

    def setNamespace(self, prefix, namespace):
        """
        Define (or redefine) a namespace 'namespace' for 'prefix'
        """
        self._get_namespaces_map()[prefix.lower()] = namespace

    def removeNamespace(self, prefix):
        """
        Remove a namespace association with 'prefix'.
        """
        del self._get_namespaces_map()[prefix.lower()]

    def clearNamespaces(self):
        """
        Remove all namespace declarations.
        """
        self._get_namespaces_map().clear()



