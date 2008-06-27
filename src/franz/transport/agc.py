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

##class AGC(object):
 
TAG_START = 128
TAG_INT_START = 128
TAG_INT_END = 192
TAG_INT_MASK = 31
TAG_SIGN_MASK = 32
TAG_NULL = 192
TAG_BYTE = 193
TAG_SHORT = 194
TAG_INT = 195
TAG_LONG = 128
TAG_CHAR = 196
TAG_FLOAT = 197
TAG_DOUBLE = 198
TAG_open7 = 199
TAG_SEQ = 200
TAG_SPARSE = 201
TAG_OBJECT = 202
TAG_TRUE = 203
TAG_FALSE = 204
TAG_OPENd = 205
TAG_OP = 206
TAG_ENDER = 207
TAG_BYTES = 210
TAG_UPI = 211
TAG_DUP = 212
TAG_REP = 213
TAG_LSTR = 222
TAG_STRING = 222
TAG_FRAG = 223
TAG_SSTR_START = 224
TAG_SSTR_END = 256
TAG_END = 256
TAG_IMM_TOP = 23
TAG_SSTR_MAX = 32
TAG_FRAG_MIN = 5
TAG_RUN_MIN = 2
DEFAULT_BUFFER_SIZE = 1024
PORT_IDLE = 0
PORT_CLOSED = -1
PORT_REQUEST = 3
PORT_WAITING_RESPONSE = 2
PORT_WAITING_REPLY = 4
PORT_MESSAGE = 1
PORT_INVOKE = 5
PORT_WAITING_RESULT = 6
UPI_WIDTH = 12
##  Operation: :call    
##  Arguments: op-string arg...       
##    Results: value-of-call...         
OP_CALL = ":call"
OP_VERIFY = ":verify"
OP_DISCONNECT = ":disconnect"
OP_DEBUG = ":debug"
AGU_NULL_CONTEXT = -2
AGU_WILD = -1
AGU_MAP_QUERY = 1
AGU_MAP_ADD = 11
AGU_MAP_REP = 21
AGU_IQ_COUNT = 1
AGU_IQ_CHUNKS = 2
AGU_IQ_UNTHRESH = 3
AGU_IQ_CHTHRESH = 4
AGU_IQ_FLAVORS = 5
AGU_IS_UNTHRESH = 13
AGU_IS_CHTHRESH = 14
AGU_IA_FLAVORS = 15
AGU_ID_FLAVORS = 25
AGU_IS_FLAVORS = 35
AGU_IQ_ALL = 31
AGU_ANON_NAME = "anon"
AGU_NODE_NAME = "node"
AGU_LITERAL_NAME = "literal"
AGU_LITERAL_LANG_NAME = "literal/lang"
AGU_TYPED_LITERAL_NAME = "typed-literal"
AGU_TRIPLE_NAME = "triple"
AGU_DEFAULT_GRAPH_NAME = "default-graph"
AGU_ENCODED_STRING_NAME = "encoded-string"
AGU_ENCODED_INTEGER_NAME = "encoded-integer"
AGU_ENCODED_FLOAT_NAME = "encoded-float"
AGU_ENCODED_TRIPLE_ID_NAME = "encoded-triple-id"
AGU_UNKNOWN_NAME = "unknown"
AGU_ANON = 1
AGU_NODE = 2
AGU_LITERAL = 3
AGU_LITERAL_LANG = 4
AGU_TYPED_LITERAL = 5
AGU_TRIPLE = 6
AGU_DEFAULT_GRAPH = 7
AGU_ENCODED_STRING = 8
AGU_ENCODED_INTEGER = 9
AGU_ENCODED_FLOAT = 10
AGU_ENCODED_TRIPLE_ID = 11
AGU_UNKNOWN = 0
AGU_PROTOCOL_LEVEL = 7
AG_DIRECT_LEVEL = 2
AG_ACCESS_TRIPLE_STORE = "db.agraph.servers::ag-access-triple-store"
AG_EXISTS_P = "db.agraph.servers::ag-exists-p"
AGJ_EVAL = "db.agraph.servers::agj-eval"
AGJ_EVAL_A = "agj-eval" ;
AGJ_BIND = "db.agraph.servers::agj-bind"
AGJ_TRACE = "db.agraph:agj-trace"
AG_SERVER_TRACE = "db.agraph:ag-server-trace"
AGJ_TRACE_INT = "db.agraph.servers::agj-trace-int"
AGJ_TRACE_INT_A = "agj-trace-int" ;
AGJ_NAMESPACES = "db.agraph.servers::agj-namespaces"
AGJ_NAMESPACES_A = "agj-namespaces"
AGJ_SERVER_OPTIONS = "db.agraph.servers::agj-server-options"
AG_STOP_AGJ_APPLICATION = "db.agraph.servers::stop-agj-application"
AG_APPLY = "db.agraph.servers::ag-apply"
AG_SYNC = "ag-sync"
AG_CLOSE = "ag-close"
AG_LOAD = "ag-load"
AG_RDF = "ag-rdf"
AG_NUMBER = "ag-number"
AG_INDEX = "ag-index"
AG_ALL = "ag-all"
AG_GET_TRIPLES = "ag-get-triples"
AG_GET_TRIPLE_RANGE = "ag-get-triple-range"
AG_INFER_TRIPLES = "ag-infer-triples"
AG_INFER_TRIPLE_RANGE = "ag-infer-triple-range"
AG_NEXT_WITH_PARTS = "ag-next-with-parts"
AG_NEXT = "ag-next"
AG_ADD_PART = "ag-add-part"
AG_ADD_TRIPLE = "ag-add-triple"
AG_ADD_TRIPLES = "ag-add-triples"
AG_DELETE = "ag-delete"
AG_INTERN_RES = "ag-intern-res"
AG_INTERN_LIT = "ag-intern-lit"
AG_NEW_NODE = "ag-new-node"
AG_GET_NODE_PARTS = "ag-get-node-parts"
AG_GET_TRIPLE_PARTS = "ag-get-triple-parts"
AG_SELECT_TRIPLES = "ag-select-triples"
AG_SELECT_VALUES = "ag-select-values"
AG_TWINQL_ASK = "ag-twinql-ask"
AG_TWINQL_FIND = "ag-twinql-find"
AG_TWINQL_QUERY = "ag-twinql-query"
AG_TWINQL_SELECT = "ag-twinql-select"
AG_DISCARD_CURSOR = "ag-discard-cursor"
AG_INDEXING = "ag-indexing"
AG_MAPPING = "ag-mapping"
AG_FREETEXT_PREDICATES = "ag-freetext-predicates"
AG_FREETEXT_STATEMENTS = "ag-freetext-statements"
AG_FREETEXT_SUBJECTS = "ag-freetext-subjects"
AG_FEDERATE = "db.agraph.servers::ag-federate"
AG_FIND_STORE = "db.agraph.servers::ag-find-store"
AG_GET_STORES = "ag-get-stores"
AG_SERIALIZE_TRIPLES = "ag-serialize-triples"
AG_REGISTER_GENERATOR = "ag-register-generator"
AG_COPY_GENERATORS = "ag-copy-generators"
AG_BREADTH_FIRST_SEARCH = "ag-breadth-first-search"
AG_DEPTH_FIRST_SEARCH = "ag-depth-first-search"
AG_BIDIRECTIONAL_SEARCH = "ag-bidirectional-search"
AG_BREADTH_FIRST_MAP = "ag-breadth-first-map"
AG_DEPTH_FIRST_MAP = "ag-depth-first-map"
AG_BIDIRECTIONAL_MAP = "ag-bidirectional-map"
AG_BREADTH_FIRST_ALL = "ag-breadth-first-all"
AG_DEPTH_FIRST_ALL = "ag-depth-first-all"
AG_BIDIRECTIONAL_ALL = "ag-bidirectional-all"
AG_BREADTH_FIRST_BIPARTITE = "ag-breadth-first-bipartite"
AG_DEPTH_FIRST_BIPARTITE = "ag-depth-first-bipartite"
AG_BIDIRECTIONAL_BIPARTITE = "ag-bidirectional-bipartite"
AG_IN_NEIGHBORS = "ag-in-neighbors"
AG_OUT_NEIGHBORS = "ag-out-neighbors"
AG_NODAL_NEIGHBORS = "ag-nodal-neighbors"
AG_IN_DEGREE = "ag-in-degree"
AG_OUT_DEGREE = "ag-out-degree"
AG_NODAL_DEGREE = "ag-nodal-degree"
AG_EGO_GROUP = "ag-ego-group"
AG_DENSITY = "ag-density"
AG_DEGREE_CENTRALITY = "ag-degree-centrality"
AG_CLOSENESS_CENTRALITY = "ag-closeness-centrality"
AG_BETWEENNESS_CENTRALITY = "ag-betweenness-centrality"
AG_IS_CLIQUE = "ag-is-clique"
AG_CLIQUES = "ag-cliques"
AG_MAP_CLIQUES = "ag-map-cliques"
AG_REGISTER_STRIPING = "ag-register-striping"
AG_ADD_SUBTYPE = "ag-add-subtype"
AG_ENCODE_GEOUPI = "ag-encode-geoupi"
AG_DECODE_GEOUPI = "ag-decode-geoupi"
AG_GET_GEO_IN_BOX = "ag-get-geo-in-box"
AG_GET_GEO_TRIPLES = "ag-get-geo-triples"
AG_ADD_POLYGON = "ag-add-polygon"
AG_GET_POLYGON = "ag-get-polygon"
AG_POINTS_INSIDE_POLY = "ag-points-inside-poly"
AG_ADD_GEO_MAPPING = "ag-add-geo-mapping"

