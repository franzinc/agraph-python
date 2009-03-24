#!/usr/bin/env python

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
from franz.openrdf.vocabulary.xmlschema import XMLSchema
import datetime
import time

XSD = XMLSchema

###########################################################################################################
## RDF Constants
###########################################################################################################

#class XSD:
#    NS = "http://www.w3.org/2001/XMLSchema#"
#    STRING = NS + "string"
#    BOOLEAN = NS + "boolean"
#    INTEGER = NS + "integer"
#    INT = NS + "int"
#    LONG = NS + "long"    
#    FLOAT = NS + "float"        
#    DOUBLE = NS + "double"
#    DATE = NS + "date"
#    DATETIME = NS + "datetime"
#    TIME = NS + "time"
#    NUMBER = NS + 'number' ## NOT SURE ABOUT THIS
#    URL = NS + 'anyURI'
    
###########################################################################################################
## Exceptions
###########################################################################################################

###########################################################################################################
## Tokenizer
###########################################################################################################

CONTEXTS_OR_DATASET = 'CONTEXTS'

ALIASES = {
    'CONTEXT': 'CONTEXTS',
    'TPL': 'TRIPLE',
    'IN': 'MEMBER',
    }

class Token:
    VARIABLE = 'VARIABLE'
    STRING = 'STRING'
    QNAME = 'QNAME'
    URI = 'URI'
    BRACKET = 'BRACKET'
    RESERVED_WORD = 'RESERVED_WORD'
    NUMBER = 'NUMBER'
    BRACKET_SET = set(['(', ')', '[', ']',])
    RESERVED_WORD_SET = set(['AND', 'OR', 'NOT', 'OPTIONAL', 'MEMBER', 'TRUE', 'FALSE', 'LIST',
                             '=', '<', '>', '<=', '>=', '!=', '+', '-', 'TRIPLE', 'QUAD',
                             'SELECT', 'DISTINCT', 'WHERE', CONTEXTS_OR_DATASET, 'LIMIT',
                             'REGEX',])
    
    def __init__(self, token_type, value):
        if token_type == Token.RESERVED_WORD:
            value = OpExpression.parse_operator(value)
        self.value = value
        self.token_type = token_type
        self.offset = -1
        
    def __str__(self):
        if self.token_type == Token.VARIABLE:
            return '?' + self.value
        elif self.token_type == Token.STRING:
            return '"%s"' % self.value
        else:
            return self.value
        
    def token_is(self, value):
        return self.value.upper() == value.upper()
        
    @staticmethod
    def reserved_type(token, token_types):
        if not token: return False
        if isinstance(token, Term): return False
        if not token.token_type == Token.RESERVED_WORD: return False
        token_types = token_types if isinstance(token_types, (list, tuple, set)) else [token_types]
        for t in token_types:
            if token.value == t: return True
        return False
    
    @staticmethod
    def printem(message, tokens):
        them = [str(tok) for tok in tokens]
        print message + str(them)

ATOMIC_TERM_TOKEN_TYPES = set([Token.VARIABLE, Token.STRING, Token.NUMBER, Token.URI, Token.QNAME])

LEGAL_VARIABLE_CHARS = set(['.', '_', '-',])

class Tokenizer():
    def __init__(self, translator, source_string):
        self.translator = translator
        self.source_string = source_string
        self.tokens = []
        
    def grab_variable(self, string):
        """
        'string' begins with a question mark, i.e., it begins with a variable.
        Convert it into a variable token, and return the remainder of the string
        """
        endPos = 9999
        for i in range(1, len(string)):
            c = string[i]
            if c.isalnum(): continue
            if c in LEGAL_VARIABLE_CHARS: continue
            endPos = i
            break
        token = Token(Token.VARIABLE, string[1:endPos])
        self.tokens.append(token)
        return string[endPos:] if endPos < 9999 else ''
    
    def grab_string(self, string, delimiter):
        """
        'string' begins with a single or double quote.
        Convert the quoted portions it into a string token, and return the remainder of the string.
        """
        endPos = -1
        for i in range(1, len(string)):
            c = string[i]
            if c == delimiter:
                endPos = i
                break
        if endPos == -1:
            raise QuerySyntaxException("Unterminated string: %s" % string)
        token = Token(Token.STRING, string[1:endPos])
        self.tokens.append(token)
        return string[endPos + 1:]
    
    def grab_uri(self, string):
        """
        'string' begins with a '<'.
        Convert the URI portion into a URI token, and return the remainder of the string.
        """
        endPos = -1
        for i in range(0, len(string)):
            c = string[i]
            if c == '>':
                endPos = i
                break
        if endPos == -1:
            raise QuerySyntaxException("Unterminated URI: %s" % string)
        token = Token(Token.URI, string[1:endPos])
        self.tokens.append(token)
        return string[endPos + 1:]
    
    DELIMITER_CHARS = set([' ', ',', '(', ')', '[', ']'])
    
    def grab_delimited_word(self, string):
        """
        The first token in 'string must be delimited by a blank,
        ## comma, or other delimiter.  Find the end, and convert the
        prefix in between into a token.  Or convert the entire thing
        into a token
        """
        endPos = -1
        for i in range(0, len(string)):
            c = string[i]
            if c in Tokenizer.DELIMITER_CHARS:
                endPos = i
                break
        word = string[:endPos] if endPos >= 0 else string
        word = ALIASES[word.upper()] if ALIASES.get(word.upper()) else word
        if word.upper() in Token.RESERVED_WORD_SET:
            self.tokens.append(Token(Token.RESERVED_WORD, word))
        elif word[0].isdigit():
            self.tokens.append(Token(Token.NUMBER, word))
        elif word.find(':') >=0:
            self.tokens.append(Token(Token.QNAME, word))
        else:
            self.translator.syntax_exception("Unrecognized term '%s'" % word, self.tokens[len(self.tokens) - 1])
        if endPos == -1:
            ## ran off the end of the string:
            return ''
        else:
            return string[endPos:]
    
    def super_strip(self, string):
        """
        Strip blanks AND leading commas.
        """
        string = string.strip()
        beginPos = 0
        for i in range(0, len(string)):
            c = string[i]
            if c == ' ' or c == ',':
                beginPos += 1
            else:
                break
        return string[beginPos:]
    
    def tokenize_next(self, string):
        """
        Parse 'string' into tokens.  Push tokens into 'tokens' during recursion,
        and return 'tokens'.
        """
        string = self.super_strip(string)
        if not string or string == ' ': return ''
        c = string[0]
        if c == '?':
            suffix = self.grab_variable(string)
        elif c in Token.BRACKET_SET:
            self.tokens.append(Token(Token.BRACKET, c))
            suffix = string[1:]
        elif c in ['"', "'"]:
            suffix = self.grab_string(string, c)
        elif c == '<' and string[1].isalpha():
            suffix = self.grab_uri(string)
        ## at this point, the first token must be delimited by a blank,
        ## comma, or delimiter.  Find the end:
        else:
            #print "GRAB DELIMITED", string
            suffix = self.grab_delimited_word(string)
            #print "   SUFFIX '%s'" % suffix, "TOKEN", tokens[len(tokens) - 1]
        newToken = self.tokens[len(self.tokens) - 1]
        newToken.offset = len(self.source_string) - len(suffix) - len(newToken.value)
        return suffix

    def tokenize(self):
        suffix = self.source_string
        while suffix:
            suffix = self.tokenize_next(suffix)
        #print "TOKENIZED", [str(t) for t in self.tokens]
        return self.tokens
    
    @staticmethod
    def tokens_to_string(tokens, comma_delimited=False):
        strings = [str(tok) for tok in tokens]
        return ', '.join(strings) if comma_delimited else ' '.join(strings)
    

###########################################################################################################
##
###########################################################################################################
    
SELECT = 'select'
DISTINCT = 'distinct'
FROM = 'from'
WHERE = 'where'
LIMIT = 'limit'

class QueryBlock:
    def __init__(self, query_type=SELECT):
        query_type = query_type
        self.select_terms = None
        self.where_clause = None
        self.contexts_clause = None
        self.distinct = False
        self.limit = -1
        self.temporary_enumerations = {}
        
    def stringify(self, newlines=False): 
        newline = '\n' if newlines else ''
        return """select %(select)s %(newline)swhere %(where)s""" % {
                        'select': stringify_terms(self.select_terms), 'where': self.where_clause, 'newline': newline,}
    
    def __str__(self):
        return self.stringify(newlines=True)
        
def stringify_terms(terms, comma_delimited=False):
    strings = [str(term) for term in terms]
    return ', '.join(strings) if comma_delimited else ' '.join(strings)
        
class Term:
    RESOURCE = 'RESOURCE'
    LITERAL = 'LITERAL'
    VARIABLE = 'VARIABLE'
    ARTIFICIAL = 'ARTIFICIAL'
    def __init__(self, term_type, value, datatype=None, qname=None):
        self.term_type = term_type
        self.value = value
        self.datatype = datatype
        self.qname = qname
        
    def clone(self):
        """
        Shallow copy of 'self'.
        """
        term = Term(self.term_type, self.value, self.datatype, self.qname)
        return term
    
    def __str__(self): 
        if self.term_type == Term.RESOURCE:
            ## TODO: UPGRADE TO HANDLE PREFIXES
            if self.qname:
                return self.qname
            else:
                return "<%s>" % self.value
        elif self.term_type == Term.VARIABLE:
            return '?' + self.value
        elif self.term_type == Term.LITERAL:
            ## TODO: UPGRADE TO HANDLE TYPED LITERALS
            if self.datatype == XSD.STRING:
                return '"%s"' % self.value
            elif self.datatype in [XSD.INTEGER, XSD.NUMBER]:
                return self.value
            else:
                return '"%s"<%s>' % (self.value, self.datatype)
        elif self.term_type == Term.ARTIFICIAL:
            return self.value

ARTIFICIAL_TERMS = set(['TRIPLE', 'QUAD'])
ARITHMETIC_OPERATORS = set(['+', '-'])
COMPARISON_OPERATORS = set(['=', '<', '>', '<=', '>=', '!='])
PREFIX_OPERATORS = set(['REGEX'])
UNARY_BOOLEAN_OPERATORS = set(['NOT', 'OPTIONAL'])
BOOLEAN_OPERATORS = set(['AND', 'OR', 'MEMBER']).union(UNARY_BOOLEAN_OPERATORS).union(COMPARISON_OPERATORS).union(PREFIX_OPERATORS)
VALUE_OPERATORS = ARITHMETIC_OPERATORS
CONNECTIVE_OPERATORS = BOOLEAN_OPERATORS.union(VALUE_OPERATORS)
OPERATOR_EXPRESSIONS = (CONNECTIVE_OPERATORS.union(ARITHMETIC_OPERATORS).union(ARTIFICIAL_TERMS)
                        .union(set(['ENUMERATION', 'TRUE', 'FALSE', 'LIST', ]))) # omits 'PREDICATION'

class OpExpression:
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'
    IN = 'MEMBER'
    OPTIONAL = 'OPTIONAL'    
    ENUMERATION = 'ENUMERATION'
    PREDICATION = 'PREDICATION'
    TRUE = 'TRUE'
    FALSE = 'FALSE'
    EQUALITY = '='
    TRIPLE = 'TRIPLE'
    QUAD = 'QUAD'
    ## MAY NOT NEED TO BE EXPLICIT ABOUT THESE:
#    PLUS = '+'
#    MINUS = '-'
    ## normalization introduces addition expression types:
    COMPUTE = 'COMPUTE'
    
    def __init__(self, operator, arguments):
        self.operator = operator
        self.arguments = arguments
        self.predicate = None
        self.is_spo = False
        self.context = None
        self.parent = None
        if not isinstance(arguments, list):
            print "BREAK HERE"
    
    @staticmethod
    def parse_operator(value):
        value = value.upper()
        if not value in OPERATOR_EXPRESSIONS and not value in ['SELECT', 'DISTINCT', 'WHERE', 'CONTEXTS', 'LIMIT']:
            raise Exception("Need to add '%s' to list of OPERATOR_EXPRESSIONS" % value)
        return value
    
    def clone(self):
        """
        Shallow copy of 'self'
        """
        op = OpExpression(self.operator, self.arguments)
        op.predicate = self.predicate
        op.is_spo = self.is_spo
        op.context = self.context
        return op
        
    def __str__(self):
        return str(StringsBuffer(complain='SILENT').common_logify(self))

INFIX_WITH_PREFIX_TRIPLES = True

class CommonLogicTranslator:
    """
    """
    SPARQL = 'SPARQL'
    PROLOG = 'PROLOG'
    
    def __init__(self, query=None, subject_comes_first=False):
        query = query.strip()
        self.set_source_query(query)
        self.parse_tree = None
        ## if 'subject_comes_first' is 'True', it says that all predications are in SPO order,
        ## whether prefixed by 'TRIPLE' or not:       
        self.subject_comes_first = subject_comes_first
    
    def set_source_query(self, query):        
        self.source_query = query
        self.infix_parse = query and not query[0] == '('
        self.infix_with_prefix_triples = self.infix_parse and INFIX_WITH_PREFIX_TRIPLES
        
    SHIM = 0 ## hack until we figure out why offsets are not quite right
    
    def syntax_exception(self, message, token=None):
        if isinstance(token, list):
            token = token[0] if token else None
        supplement = ''
        if token:
            supplement = "Error occurred at offset %i in the string \n    %s" % (token.offset, self.source_query)
            pointer = '    ' + ' ' * (token.offset + 1 + CommonLogicTranslator.SHIM) + '^' 
            message = "%s\n%s\n%s" % (message, supplement, pointer) 
        raise QuerySyntaxException(message)
        
    def clean_source(self):
        self.source_query = self.source_query.replace('\n', ' ').replace('\t', ' ').strip()
        
    def token_to_term(self, token):
        if token.token_type == Token.VARIABLE:
            return Term(Term.VARIABLE, token.value)
        elif token.token_type == Token.URI:
            return Term(Term.RESOURCE, token.value)
        elif token.token_type == Token.STRING:
            return Term(Term.LITERAL, token.value, XSD.STRING)
        elif token.token_type == Token.NUMBER:
            if token.value.isdigit():
                return Term(Term.LITERAL, token.value, XSD.INTEGER)                    
            else:
                return Term(Term.LITERAL, token.value, XSD.NUMBER)        
        elif token.token_type == Token.QNAME:
            return Term(Term.RESOURCE, token.value, qname=token.value)
        elif token.token_type == Token.RESERVED_WORD and token.value in ARTIFICIAL_TERMS:
            return Term(Term.ARTIFICIAL, token.value)            
        else:
            raise Exception("Can't convert token %s to a term" % token)
 
    def normalize_resource(self, term, tokens):
        """
        Make sure resource term is legitimate (not sure why tokenizer doesn't do this)
        TODO: Check that the string really is a URI or qname
        """
        if (term.term_type == Term.RESOURCE):
            if not term.value:
                self.syntax_exception("Empty string found where resource expected.", tokens)
            if term.value[0] == '<' and self.value[len(self) - 1] == '>':
                term.value = term.value[1:-1]
            if term.value.lower().startswith("http:") or term.value.lower().startswith("ftp:"):
                term.qname = None
            else:
                term.qname = term.value
           
    def parse_select_clause(self, tokens):
        """
        Parse 'select_string' and return a list of terms.
        TODO: UPGRADE TO ALLOW ARBITRARY EXPRESSIONS HERE
        """
        isWrappedWithParens =  len(tokens) >= 2 and tokens[0].value == '(' and tokens[len(tokens) - 1].value == ')' 
        if not self.infix_parse and not isWrappedWithParens:
            self.syntax_exception("Missing parentheses around select clause arguments")
        if isWrappedWithParens:
            tokens = tokens[1:-1]           
        terms = []
        for t in tokens:
            ## TODO: ALLOW ARBITRARY EXPRESSIONS HERE: 
#            if not t.token_type in [Token.URI, Token.STRING, Token.NUMBER, Token.VARIABLE, Token.QNAME]:
#                self.syntax_exception("Illegal operator '%s' found in select clause" % t.value, t)
            terms.append(self.token_to_term(t))
        return terms
        
    def parse_enumeration(self, value, tokens):
        """
        """
        arguments = self.parse_expressions(tokens, [])
        for arg in arguments:
            self.normalize_resource(arg, tokens)
        op = OpExpression(OpExpression.ENUMERATION, arguments)
        op.predicate = value
        return op
    
    def parse_predication(self, tokens, predicate=None):
        """
        'tokens' represent a predicate applied to arguments
        """
        #print "PARSE PREDICATION", predicate, [str(t) for t in tokens]       
        if not predicate:
            predicate = tokens[0]
            tokens = tokens[1:]
        if self.infix_parse and tokens[0].value == '(':
            return self.parse_bracket(tokens, [], predicate=predicate) 
        isSPO = Token.reserved_type(predicate, [OpExpression.TRIPLE, OpExpression.QUAD])
        predicate = self.token_to_term(predicate)
        arguments = self.parse_expressions(tokens, [])
        if isSPO:
            pass
        elif self.subject_comes_first and len(arguments) in [2,3]:
            ## this is probably a bad idea; we aren't debugging it:
            temp = [predicate]
            temp.extend(arguments)
            arguments = temp
            toq = 'TRIPLE' if len(arguments) == 3 else 'QUAD'
            predicate = self.token_to_term(Token(Token.RESERVED_WORD, toq))
            isSPO = True
        else:
            if not predicate.term_type in [Term.RESOURCE, Term.VARIABLE]:
                self.syntax_exception("Found illegal term '%s' where resource expected" % predicate.value, tokens[0])
            isSPO = False
        if isSPO:
            predicate = OpExpression.QUAD if len(arguments) == 4 else OpExpression.TRIPLE
        op = OpExpression(OpExpression.PREDICATION, arguments)
        op.predicate = predicate
        op.is_spo = isSPO
        return op
                   
    def parse_bracketed_expression(self, tokens, bracket, predicate=None, is_boolean=None):
        """
        'tokens' are encoded in a bracket beginning with 'bracket'.
        Figure out what kind of expression we have, and parse it.
        """
        if bracket.value == '[':
            return self.parse_enumeration('LIST', tokens)
        ## must be parenthesized expression
        if not tokens:
            self.syntax_exception("Found empty set of parentheses", tokens)
        ## check if its a predication:
        allAtomicArguments = True
        for t in tokens:
            if not t.token_type in ATOMIC_TERM_TOKEN_TYPES:
                allAtomicArguments = False
                break                
        if allAtomicArguments:
            return self.parse_predication(tokens, predicate=predicate)
        elif  Token.reserved_type(tokens[0], [OpExpression.TRIPLE, OpExpression.QUAD]):
            if self.infix_parse:
                ## EXPERIMENT:                
                return self.parse_infix_expression(tokens, [], connective=None, needs_another_argument=True, is_boolean=True)
            else:
                return self.parse_predication(tokens)
        elif predicate:
            self.syntax_exception("MAYBE A FUNCTION, BUT NOT LEGAL PREDICATION", tokens)
        ## we don't know yet what we have here:
        beginToken = tokens[0]
        tokenType = beginToken.token_type
        value = beginToken.value
        if not self.infix_parse and tokenType == Token.BRACKET and value == '(':
            ## see if last token is closing bracket; otherwise, its illegal:
            endToken = tokens[len(tokens) - 1]
            if endToken.token_type == Token.BRACKET and endToken.value == ')':
                return self.parse_bracketed_expression(tokens[1:-1], beginToken, is_boolean=is_boolean)
            else:
                self.syntax_exception("Found parenthesized expression where term expected: '%s'" % Tokenizer.tokens_to_string(tokens), beginToken) 
        elif tokenType == Token.RESERVED_WORD:
            if value in CONNECTIVE_OPERATORS:
                if self.infix_parse and not value in UNARY_BOOLEAN_OPERATORS:
                    self.syntax_exception("Found operator '%s' where term expected" % Tokenizer.tokens_to_string(tokens), beginToken)
                if value in BOOLEAN_OPERATORS and not is_boolean:
                    self.syntax_exception("Found boolean expression where value expression expected '%s'" % Tokenizer.tokens_to_string(tokens), beginToken)
                if value in VALUE_OPERATORS and is_boolean:
                    self.syntax_exception("Found value expression where boolean expression expected '%s'" % Tokenizer.tokens_to_string(tokens), beginToken)
                ## NOT SURE ABOUT THIS (ESPECIALLY FOR INFIX):
                isBoolean = (value in BOOLEAN_OPERATORS and not value in COMPARISON_OPERATORS and not value in PREFIX_OPERATORS)
                arguments = self.parse_expressions(tokens[1:], [], is_boolean=isBoolean)
                return OpExpression(value, arguments)
            elif value in [OpExpression.TRUE, OpExpression.FALSE]:
                if not is_boolean:
                    self.syntax_exception("Found boolean expression where term expected '%s'" % Tokenizer.tokens_to_string(tokens), beginToken)
                elif len(tokens) > 1:
                    self.syntax_exception("Found bizarre expression '%s'" % Tokenizer.tokens_to_string(tokens), beginToken)
                return OpExpression(value, [])
            elif value in ['LIST', 'SET', 'BAG']:
                return self.parse_enumeration(value, tokens[1:])
            else:
                raise Exception("Unimplemented reserved word '%s'" % value)
        if self.infix_parse:
            return self.parse_infix_expression(tokens, [], connective=None, needs_another_argument=True, is_boolean='UNKNOWN')
        elif is_boolean is True:
                ## THIS ERROR MESSAGE WORKED ONCE.  NEED TO FIND OUT IF IT'S TOO SPECIFIC:
                self.syntax_exception("Illegal expression %s where prefix expression expected"  % Tokenizer.tokens_to_string(tokens), tokens)
        else:
            ## THIS IS BOGUS SO FAR.  
            return self.parse_function_expression(tokens)    
        
    def parse_unary_connective(self, tokens, expressions, connective=None):
        """
        Infix mode needs special handling for the NOT operator (because its a prefix operator).
        TODO: FIGURE OUT IF 'OPTIONAL' NEEDS SIMILAR HANDLING HERE
        TODO: GENERALIZE TO ALSO HANDLE 'MEMBER'???
        """
        opName = tokens[0].value
        if len(tokens) < 2:
            self.syntax_exception("Insufficient arguments to '%s' operator" % opName.lower(), tokens)
        beginToken = tokens[1]
        beginVal = beginToken.value
        if beginVal == '(':
            return self.parse_bracket(tokens[1:], expressions, connective=connective, 
                                      unary_operator=opName, is_boolean=True)
        else:
            self.syntax_exception("'%s' operator expects a parenthesized argument" % opName.lower(), tokens)        
    
    def parse_bracket(self, tokens, expressions, is_boolean=False, connective=None, unary_operator=None, predicate=None):
        """
        'tokens' begins with a bracket.  Find the end bracket, and convert the tokens
        in between into an expression.  Recursively parse the remaining expressions.
        """
        beginBracket = tokens[0]
        beginVal = beginBracket.value
        endVal = None
        if beginVal == '(': endVal = ')'
        elif beginVal == '[': endVal = ']'
        nestingCounter = 0
        for i, tok in enumerate(tokens):
            if tok.token_type == Token.BRACKET:
                if tok.value == beginVal: nestingCounter += 1
                elif tok.value == endVal: nestingCounter -= 1
            if nestingCounter == 0:
                exp = self.parse_bracketed_expression(tokens[1:i], beginBracket, is_boolean=is_boolean, predicate=predicate)
                if unary_operator:
                    exp = OpExpression(unary_operator, [exp])
                expressions.append(exp)
                if self.infix_parse:
                    return self.parse_infix_expression(tokens[i + 1:], expressions, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
                else:
                    return self.parse_expressions(tokens[i + 1:], expressions, is_boolean=is_boolean)   
                     
    def parse_expressions(self, tokens, expressions, is_boolean=False):
        """
        Parse 'tokens' into an expression and append the result to 'expressions'.  
        If not all tokens are used up, recursively parse expressions.
        """
        if not tokens: return expressions
        beginToken = tokens[0]
        tokenType = beginToken.token_type
        if tokenType in set([Token.STRING, Token.QNAME, Token.URI, Token.NUMBER]):
            if is_boolean is True:
                self.syntax_exception("Term found where boolean expression expected '%s'" % beginToken.value, beginToken)
            expressions.append(self.token_to_term(beginToken))
            return self.parse_expressions(tokens[1:], expressions, is_boolean=is_boolean)
        if tokenType in set([Token.VARIABLE]):
            expressions.append(self.token_to_term(beginToken))
            return self.parse_expressions(tokens[1:], expressions, is_boolean=is_boolean)
        if tokenType == Token.BRACKET:
            exps = self.parse_bracket(tokens, expressions, is_boolean=is_boolean)
            ## hack: in infix mode, 'parse_bracket' returns a singleton, which all callers except this one prefer:
            return [exps] if self.infix_parse else exps
        elif tokenType == Token.RESERVED_WORD:
            if Token.reserved_type(beginToken, [OpExpression.TRUE, OpExpression.FALSE]):
                expressions.append(OpExpression(beginToken.value, []))
                return self.parse_expressions(tokens[1:], expressions, is_boolean=is_boolean)
            elif Token.reserved_type(beginToken, [OpExpression.TRIPLE, OpExpression.QUAD]):
                ## this is a hack that insures that 'parse_predication' gets applied to this expression
                ## for an infix QUAD, it should be an error if the parenthesis is missing
                if self.infix_parse and tokens[1].value == '(':
                    return [self.parse_bracket(tokens[1:], [], predicate=beginToken, is_boolean=is_boolean)]
                else:
                    ## this handles recursion triggered just above for infix; not sure if prefix hits this
                    expressions.append(self.token_to_term(beginToken))
                    return self.parse_expressions(tokens[1:], expressions, is_boolean=is_boolean)
        ## failure
        if is_boolean is True:
            self.syntax_exception("Found illegal term '%s' where boolean expression expected" % beginToken.value, beginToken)
        else:
            self.syntax_exception("Found illegal term '%s'" % str(beginToken), beginToken)
 
 
    def parse_infix_expression(self, tokens, arguments, connective=None, needs_another_argument=True, is_boolean=False):
        """
        Parse 'tokens' into an expression and return the result.
        """
        if not tokens:
            if needs_another_argument:
                if not connective:
                    self.syntax_exception("Found nothing where term expected (NOT A GOOD ERROR MESSAGE BECAUSE NO CONTEXT)")
                else:
                    self.syntax_exception("%s connective expects another argument (NOT A GOOD ERROR MESSAGE BECAUSE NO CONTEXT)" % connective)
            elif connective:
                return OpExpression(connective, arguments)
            elif len(arguments) == 1:
                return arguments[0]
            else: 
                ## WE DON'T UNDERSTAND THIS CASE YET:
                raise Exception("BUG -- parse_infix expression went bizarro")
        beginToken = tokens[0]
        tokenType = beginToken.token_type
        if needs_another_argument:
            ## the next argument must be a term (not a connective)
            if self.infix_with_prefix_triples:
                if tokenType in [Token.URI, Token.QNAME] and len(tokens) > 1 and tokens[1].value == '(':
                    predicate = self.token_to_term(beginToken)
                    return self.parse_bracket(tokens[1:], arguments, connective=connective, predicate=predicate, is_boolean=is_boolean)
                elif Token.reserved_type(beginToken, [OpExpression.TRIPLE, OpExpression.QUAD]):
                    if tokens[1].value == '(':
                        return self.parse_bracket(tokens[1:], arguments, connective=connective, predicate=beginToken, is_boolean=is_boolean)
                    else:
                        self.syntax_exception("Expected '(' but found '{0}'".format(tokens[1]), tokens)
            if tokenType in ATOMIC_TERM_TOKEN_TYPES:
#                if is_boolean is True:
#                    self.syntax_exception("Value term found where boolean expression expected '%s'" % beginToken.value, beginToken)
                arguments.append(self.token_to_term(beginToken))
                return self.parse_infix_expression(tokens[1:], arguments, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
            if tokenType in set([Token.VARIABLE]):
                arguments.append(self.token_to_term(beginToken))
                return self.parse_infix_expression(tokens[1:], arguments, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
            if tokenType == Token.BRACKET:
                return self.parse_bracket(tokens, arguments, connective=connective, is_boolean=is_boolean)
            elif Token.reserved_type(beginToken, [OpExpression.NOT, OpExpression.OPTIONAL]) and is_boolean:
                return self.parse_unary_connective(tokens, arguments, connective=connective)
            elif Token.reserved_type(beginToken, [OpExpression.TRUE, OpExpression.FALSE]) and is_boolean:
                    arguments.append(OpExpression(beginToken.value, []))
                    return self.parse_expressions(tokens[1:], arguments, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
            else: # failure
                if is_boolean is True:
                    self.syntax_exception("Found illegal term '%s' where boolean expression expected" % beginToken.value, beginToken)
                else:
                    self.syntax_exception("Found illegal term '%s'" % str(beginToken), beginToken)
        ## we have an argument; the next token MUST be a connective:
        nextConnective = beginToken.value        
        if is_boolean is True:
            if not Token.reserved_type(beginToken, BOOLEAN_OPERATORS):
                self.syntax_exception("Found '%s' where one of %s expected" % (beginToken.value, BOOLEAN_OPERATORS), beginToken)
        elif is_boolean is False:
            if not Token.reserved_type(beginToken, VALUE_OPERATORS):
                self.syntax_exception("Found '%s' where one of %s expected" % (beginToken.value, VALUE_OPERATORS), beginToken)
        else:
            if not Token.reserved_type(beginToken, CONNECTIVE_OPERATORS):
                self.syntax_exception("Found '%s' where one of %s expected" % (beginToken.value, CONNECTIVE_OPERATORS), beginToken)

        if connective and not connective == nextConnective:
            ## the next connective is different than the previous one; coalesce the
            ## previous arguments into a single argument:
            ## NOTE: THIS IS WHERE OPERATOR PRECEDENCE WOULD HAPPEN IF WE HAD IT. BUT WE DON'T HAVE IT:
            arguments = [OpExpression(connective, arguments)]
        isBoolean = nextConnective in BOOLEAN_OPERATORS and not nextConnective in COMPARISON_OPERATORS
        return self.parse_infix_expression(tokens[1:], arguments, connective=nextConnective, is_boolean=isBoolean)
   
    def validate_parentheses(self, tokens):
        balance = 0
        unmatchedLeft = None
        for t in tokens:
            if t.value == '(': 
                balance += 1
                if balance == 1:
                    unmatchedLeft = t
            elif t.value == ')': balance -= 1
            if balance < 0:
                self.syntax_exception("Unmatched right parentheses", t)
        if balance > 0:
            self.syntax_exception("Unmatched left parentheses", unmatchedLeft)
            
    def parse_where_clause(self, tokens):
        """
        Parse string into a single expression, or a list of expressions.
        If the latter, convert the list into an AND expression.
        """
        self.validate_parentheses(tokens)
        if self.infix_parse:
            expressions = [self.parse_infix_expression(tokens, [], connective=None, is_boolean=True)]
        else:
            expressions = self.parse_expressions(tokens, [], is_boolean=True)
        if not expressions:
            self.syntax_exception("Query has empty where clause")            
        elif len(expressions) == 1:
            return expressions[0]
        else:
            return OpExpression(OpExpression.AND, expressions)

    def parse_contexts_clause(self, tokens):
        """
        Read in a list of context URIs.
        """
        if not tokens:
            self.syntax_exception("Contexts clause is empty", tokens)
        if not self.infix_parse or tokens[0].value == '(':
            beginToken = tokens[0]
            endToken = tokens[len(tokens) - 1]
            if not (beginToken.value == '(' and endToken.value == ')'):
                self.syntax_exception("Begin and end parentheses needed to bracket contents of CONTEXTS clause", tokens)
            tokens = tokens[1:-1]
        contexts = []
        for token in tokens:
            if token.token_type == Token.URI or Token.QNAME:
                contexts.append(self.token_to_term(token))
            else:
                self.syntax_exception("Found term '%s' where URI or qname expected" % token.value, token)
        return contexts
            
    def parse_limit_clause(self, tokens):
        if not len(tokens) == 1:
            self.syntax_exception("Expected one argument to 'limit' operator", tokens)
        token = tokens[0]
        if token.token_type == Token.NUMBER:
            return int(token.value)
        else:
            self.syntax_exception("'limit' operator expects an integer argument", token)
            
    def parse(self):
        """
        Parse 'source_query' into a parse tree.
        FOR NOW, ASSUME ITS A 'select' QUERY
        """
        self.clean_source()
        query = self.source_query.lower()
        if not query:
            raise QuerySyntaxException("Empty CommonLogic query passed to translator")
        tokens = Tokenizer(self, self.source_query).tokenize()
        self.validate_parentheses(tokens)
        if tokens[0].token_is('('):
            if not tokens[len(tokens) - 1].token_is(')'):
                raise QuerySyntaxException("Missing right parenthesis at the end of query:\n" + query)
            tokens = tokens[1:-1]
        selectToken = tokens[0]
        if not selectToken.token_is('select'):
            self.syntax_exception("Found {0} where 'select' expected".format(selectToken.value), tokens)
        qb = QueryBlock()
        self.parse_tree = qb
        ## find the reserved word tokens that subdivide the query:
        def found_one(nextToken, word, existingToken):
            if not nextToken.token_is(word): return 
            if existingToken:
                self.syntax_exception("Multiple {0} tokens in the same query".format(nextToken.value), nextToken)
            return True
        distinctToken = None
        whereToken = None
        contextsToken = None
        limitToken = None
        for tok in tokens:
            if found_one(tok, 'where', whereToken): whereToken = tok
            if found_one(tok, 'distinct', distinctToken): distinctToken = tok
            if found_one(tok, 'limit', limitToken): limitToken = tok                        
            if found_one(tok, 'contexts', contextsToken): contextsToken = tok    
        if distinctToken:
            if not distinctToken == tokens[1]:
                self.syntax_exception("Distinct is out of place; it should appear directly after {0}".format(selectToken.value), distinctToken)
            qb.distinct = True
            tokens.remove(distinctToken)
        if not whereToken:
            self.syntax_exception("Missing where clause in {0} query".format(selectToken.value), tokens)
        if contextsToken and contextsToken.offset < whereToken.offset:
            self.syntax_exception("Contexts clause must occur after the where clause", contextsToken)
        if limitToken and limitToken.offset < whereToken.offset:
            self.syntax_exception("Limit clause must occur after the where clause", limitToken)
        for i, t in enumerate(tokens): t.index = i
        qb.select_terms = self.parse_select_clause(tokens[1:whereToken.index])
        lastWhereTokenIndex = len(tokens)
        if contextsToken:
            lastWhereTokenIndex = min(lastWhereTokenIndex, contextsToken.index)
        if limitToken: 
            lastWhereTokenIndex = min(lastWhereTokenIndex, limitToken.index)
        qb.where_clause = self.parse_where_clause(tokens[whereToken.index + 1:lastWhereTokenIndex])
        if contextsToken:
            lastContextsTokenIndex = len(tokens)
            if limitToken and limitToken.index > contextsToken.index:  
                lastContextsTokenIndex = min(lastContextsTokenIndex, limitToken.index)
            qb.contexts_clause = self.parse_contexts_clause(tokens[contextsToken.index + 1:lastContextsTokenIndex])
        if limitToken:
            lastLimitTokenIndex = len(tokens)
            if contextsToken and contextsToken.index > limitToken.index:  
                lastLimitTokenIndex = min(lastLimitTokenIndex, contextsToken.index)
            qb.limit = self.parse_limit_clause(tokens[limitToken.index + 1:lastLimitTokenIndex])
       

###########################################################################################################
## Normalization
###########################################################################################################

class Normalizer:
    def __init__(self, parse_tree, language, contexts=None):
        self.parse_tree = parse_tree
        self.language = language
        self.variable_counter = -1
        self.recompute_backlinks()
        def deanglify(context):
            return context[1:-1] if context[0] == '<' else context
        self.contexts = [deanglify(cxt) for cxt in contexts] if contexts else None
        
    def normalize(self):
        if self.language == CommonLogicTranslator.PROLOG:
            self.normalize_for_prolog()
        elif self.language == CommonLogicTranslator.SPARQL:
            self.normalize_for_sparql()
    
    def help_walk(self, node, parent, processor, types, bottom_up, external_value):
        if type(node) == str: return
        if not bottom_up and (not types or isinstance(node, types)):
            processor(node, parent, external_value)
        if isinstance(node, OpExpression):
            if node.predicate and (not types or isinstance(node.predicate, types)):
                self.help_walk(node.predicate, node, processor, types, bottom_up, external_value)
            for arg in node.arguments:
                self.help_walk(arg, node, processor, types, bottom_up, external_value)
        if bottom_up and (not types or isinstance(node, types)):
            processor(node, parent, external_value)
    
    def walk(self, processor, types=None, start_node=None, bottom_up=False, external_value=None):
        """
        Walk the parse tree; apply 'processor' to each node whose type is in 'types'.
        """
        if start_node and not start_node == self.parse_tree:
            self.help_walk(start_node, start_node.parent, processor, types, bottom_up, external_value)
            return
        ## walk select clause
        for arg in self.parse_tree.select_terms:
            self.help_walk(arg, self.parse_tree.select_terms, processor, types, bottom_up, external_value)
        ## walk where clause
        self.help_walk(self.parse_tree.where_clause, self.parse_tree, processor, types, bottom_up, external_value)
    
    def recompute_backlinks(self, where_clause_only=False):
        #print "RECOMPUTE BACKLINKS"
        def add_backlinks(node, parent, external_value):
            node.parent = parent        
        self.walk(add_backlinks, start_node=(self.parse_tree.where_clause if where_clause_only else None))
        
#    def variable_name_is_referenced(self, variable_name):
#        """
#        Return 'True' if a variable named 'variable_name' occurs somewhere in the current parse tree.
#        """      
#        foundIt = [False]
#        def doit(node, parent, external_value):
#            if node.term_type == Term.VARIABLE and node.value == variable_name:                
#                foundIt[0] = True
#        self.walk(doit, types=Term)
#        return foundIt[0]

    def get_fresh_variable(self):
        """
        """
        def bump_variable_counter(node, parent, sseellff):
            if not node.term_type == Term.VARIABLE: return
            value = node.value
            if len(value) < 2: return
            if not value.startswith('v') or not value[1:].isdigit(): return
            intVal = int(value[1:])
            sseellff.variable_counter = max(self.variable_counter, intVal)
        if self.variable_counter == -1:
            self.variable_counter = 0
            self.walk(bump_variable_counter, types=Term, external_value=self)
        self.variable_counter += 1
        freshVbl = "v{0}".format(self.variable_counter)
        return Term(Term.VARIABLE, freshVbl)
    
#    def add_backlinks(self, expression, parent):
#        expression.parent = parent
#        if isinstance(expression, Term): return # quick exit
#        if isinstance(expression, OpExpression):            
#            for arg in expression.arguments:
#                self.add_backlinks(arg, expression)
#        elif isinstance(expression, QueryBlock):
#            ## tricky: select terms list is the parent of select terms
#            for term in expression.select_terms:
#                self.add_backlinks(term, expression.select_terms)
#            ## tricky: query block is the parent of where term
#            self.add_backlinks(expression.where_clause, expression)
            
    @staticmethod
    def is_boolean(node):
        if isinstance(node, OpExpression):
            return (node.predicate or node.operator in BOOLEAN_OPERATORS or
                    node.operator in [OpExpression.TRUE, OpExpression.FALSE])
        else:
            return False
        
    def substitute_node(self, out_node, in_node):
        """
        Unlink the parent of 'out_node' from it, and link it instead to 'in_node'
        Caution: Does NOT fix up backlinks, because some transforms could be messed
        up if we do.  Instead, assumes that 'recompute_backlinks' will be called 
        afterwards.
        """
        parent = out_node.parent
        if isinstance(parent, OpExpression):
            for i, arg in enumerate(parent.arguments):
                if arg == out_node:
                    parent.arguments[i] = in_node
                    return
            if out_node == parent.predicate:
                parent.predicate = in_node
                return
        elif isinstance(parent, QueryBlock):
            if parent.where_clause == out_node:
                parent.where_clause = in_node
                return
        elif isinstance(parent, list):
            for i, arg in enumerate(list):
                if arg == out_node:
                    list[i] = in_node
                    return
            return
        raise Exception("Failed to substitute out_node '%s'" % out_node)
               
    def conjoin_to_where_clause(self, node):
        """
        And-in 'node' to the top-level of the where clause.
        """ 
        whereClause = self.parse_tree.where_clause
        if isinstance(whereClause, OpExpression) and whereClause.operator == OpExpression.AND:
            whereClause.arguments.append(node)
        else:
            andNode = OpExpression(OpExpression.AND, [whereClause, node])
            self.parse_tree.where_clause = andNode
                                                                  
    def flatten_nested_ands(self):
        flattenedSomething = False
        def flatten(node, parent, external_value):
            if not node.operator == OpExpression.AND: return
            conjuncts = []
            for arg in node.arguments:
                if isinstance(arg, OpExpression) and arg.operator == OpExpression.AND:
                    conjuncts.extend(arg.arguments)
                    flattenedSomething = True
                else:
                    conjuncts.append(arg)
            node.arguments = conjuncts
        ## flatten each nested AND we find:                           
        self.walk(flatten, types=OpExpression, bottom_up=True)
        if flattenedSomething:
            self.recompute_backlinks(where_clause_only=True)
            
    def copy_node(self, node):
        """
        Deep copy of 'node'.
        Caution: Does not call 'recompute_backlinks', but something needs to.
        """
        if isinstance(node, Term):
            return node.clone()
        else:
            op = node.clone()
            op.arguments = [self.copy_node(arg) for arg in node.arguments]
            return op
        
    def distribute_disjunction(self, or_node):
        """
        Apply deMorgan's transform to the OR node 'or_node' and its AND parent.
        """
        andParent = or_node.parent
        if (not or_node.operator == OpExpression.OR or
            not andParent.operator == OpExpression.AND):
            raise Exception("Illegal node structure in 'bubble_up_disjunction'")
        newAndNodes = []
        for disjunct in or_node.arguments:
            otherConjuncts = [self.copy_node(arg) for arg in andParent.arguments if not arg == or_node]
            otherConjuncts.append(self.copy_node(disjunct))
            newAndNodes.append(OpExpression(OpExpression.AND, otherConjuncts))
        newOrNode = OpExpression(OpExpression.OR, newAndNodes)
        self.substitute_node(andParent, newOrNode)
        self.recompute_backlinks(where_clause_only=True)
 
    ###########################################################################################################
    ## Constant folding
    ###########################################################################################################
    
    ## implicit variable scoping makes this difficult
    ## we assume rather narrow scoping at first pass:
    def propagate_constants_to_predications(self, skip_context_variables=None):
        constantEqualities = []
        def collect_constant_equalities(node, parent, external_value):
            if node.operator == OpExpression.EQUALITY and isinstance(parent, OpExpression) and parent.operator == OpExpression.AND:
                variable = None
                constant = None
                for arg in node.arguments:
                    if isinstance(arg, Term):
                        if arg.term_type == Term.VARIABLE: variable = arg
                        elif arg.term_type in [Term.RESOURCE, Term.LITERAL]: constant = arg
                if variable and constant:
                    constantEqualities.append((parent, variable, constant))
        ## collect AND nodes containing variables set to constants:                                      
        self.walk(collect_constant_equalities, types=OpExpression)
        def substitute_constant_for_variable(node, parent, external_value):
            if node.operator == OpExpression.PREDICATION:
                vbl = external_value[1]
                constant = external_value[2]
                for i, arg in enumerate(node.arguments):
                    ## SPARQL can't handle constants in context position:
                    if node.is_spo and i == 3 and skip_context_variables: continue
                    if not isinstance(arg, Term): continue
                    if arg.value == vbl.value:
                        node.arguments[i] = constant
        ## search for predications AND'ed to the constant equalities,
        ## and subtitute in the corresponding constants:
        for triple in constantEqualities:
            self.walk(substitute_constant_for_variable, types=OpExpression, start_node=triple[0], external_value=triple)
            
    def is_unique_variable_within_where_clause(self, variable):
        """
        Return 'True' if the variable 'variable' occurs at most once in the
        where clause of the current parse tree.
        """      
        appearancesCounter = [0]
        def doit(node, parent, external_value):
            if node.term_type == Term.VARIABLE and node.value == variable.value:                
                appearancesCounter[0] = appearancesCounter[0] + 1
        self.walk(doit, types=Term, start_node=self.parse_tree.where_clause)
        return appearancesCounter[0] <= 1 
    

    ###########################################################################################################
    ## Specialized conversions
    ###########################################################################################################
           
    def find_value_computation_roots(self, node):
        """
        Return a list of nodes representing non-boolean computations
        within 'node' not nested within higher value computations.
        """
        roots = []
        if isinstance(node, OpExpression):
            if Normalizer.is_boolean(node):
                for arg in node.arguments:
                    roots.extend(self.find_value_computation_roots(arg))
            elif not node.operator == OpExpression.COMPUTE:
                return [node]
        return roots
    
    def flatten_one_value_computation(self, root):
        freshVar = self.get_fresh_variable()
        computeArgs = [freshVar, root.operator]
        computeArgs.extend(root.arguments)
        computeNode = OpExpression(OpExpression.COMPUTE, computeArgs)
        self.substitute_node(root, freshVar)
        parentNode = root.parent
        andNode = OpExpression(OpExpression.AND, [computeNode, parentNode])
        self.substitute_node(parentNode, andNode)
        self.recompute_backlinks(where_clause_only=True)        
       
    def flatten_value_computations(self):
        """
        Replace value operators by COMPUTE nodes
        Call this AFTER calling 'flatten_select_terms'
        """
        roots = self.find_value_computation_roots(self.parse_tree.where_clause)
        if not roots: return
        for root in roots:
            self.flatten_one_value_computation(root)
        ## nested ands are a possible by-product of value computation flattening
        self.flatten_nested_ands()            
        ## recursively flatten until no more flattening occurs
        self.flatten_value_computations()
            
    def flatten_select_terms(self):
        roots = [term for term in self.parse_tree.select_terms 
                    if isinstance(term, OpExpression) and not Normalizer.is_boolean(term)]
        if not roots: return
        newEqualities = []
        for r in roots[:]:
            freshVbl = self.get_fresh_variable()
            self.substitute_node(r, freshVbl)
            equalityOp = OpExpression(OpExpression.EQUALITY, [freshVbl, r])
            newEqualities.append(equalityOp)
        for ne in newEqualities:            
            self.conjoin_to_where_clause(ne)
        self.recompute_backlinks()
        
    def get_null_term(self):
        return Term(Term.LITERAL, "Null")
        
    def translate_optionals(self, p_or_true=False):
        def doit(node, parent, sseellff):
            if not node.operator == OpExpression.OPTIONAL: return
            arg = node.arguments[0]
            argCopy = sseellff.copy_node(arg)
            if p_or_true:
                notP = OpExpression(OpExpression.TRUE, [])
            else:
                notP = OpExpression(OpExpression.NOT, [argCopy])
            pOrNotP = OpExpression(OpExpression.OR, [arg, notP])
            self.substitute_node(node, pOrNotP)
        self.walk(doit, types=OpExpression, external_value=self)
        
    def translate_in_enumerate_into_disjunction_of_equalities(self):
        didIt = [False]
        def doit(node, parent, external_value):
            if node.operator == OpExpression.IN:
                ## assumes that the second arg is an enumeration:
                vbl = node.arguments[0]
                enumeration = node.arguments[1]
                equalities = [OpExpression(OpExpression.EQUALITY, [vbl, item]) for item in enumeration.arguments]
                orOp = OpExpression(OpExpression.OR, equalities)
                self.substitute_node(node, orOp)
                didIt[0] = True
        self.walk(doit, types=OpExpression)
        if didIt[0]:
            self.recompute_backlinks()
    
    def translate_in_enumerate_into_temporary_join(self):
        """
        If the parse tree contains enumeration tests,
        record the needed temporary relations in 'parse_tree.temporary_enumerations'
        and 
        """
        didIt = [False]
        def doit( node, parent, sseellff):
            if not node.operator == OpExpression.IN: return
            ## assumes that the second arg is an enumeration:
            vbl = node.arguments[0]
            enumeration = node.arguments[1]
            tempRelation = "<http://enumerationhack#t{0:f}>".format(time.time())
            freshVbl = sseellff.get_fresh_variable()
            joinNode = OpExpression(OpExpression.PREDICATION, [freshVbl, Term(Term.RESOURCE, tempRelation[1:-1]), vbl])
            joinNode.predicate = OpExpression.TRIPLE
            joinNode.is_spo = True
            self.substitute_node(node, joinNode)
            self.parse_tree.temporary_enumerations[tempRelation] = [str(item) for item in enumeration.arguments]            
            didIt[0] = True
        self.walk(doit, types=OpExpression, external_value=self)
        if didIt[0]:
            self.recompute_backlinks()

    ###########################################################################################################
    ## PROLOG-specific
    ###########################################################################################################
    
    def filter_quad_contexts(self):
        """
        Visit each spo quad containing a variable context argument, and wrap a filter 
        clause around it that restricts the context argument to only contexts in the 
        explicitly-specified contexts.
        """
        def doIt(node, parent, sseellff):
            if node.is_spo and len(node.arguments) == 4:
                cxt = node.arguments[3]
                if not isinstance(cxt, Term) or not cxt.term_type == Term.VARIABLE: return
                contexts = sseellff.contexts
                print "CONTEXTS", [item for item in contexts]
                equalities = [OpExpression(OpExpression.EQUALITY, [cxt, Term(Term.RESOURCE, item)]) for item in contexts]
                orOp = OpExpression(OpExpression.OR, equalities) if len(contexts) > 1 else equalities[0]
                andOp = OpExpression(OpExpression.AND, [node, orOp])
                self.substitute_node(node, andOp)
        self.walk(doIt, OpExpression, external_value=self)
    
    def quadify_triples(self):
        """
        Convert triples to quads everywhere, inserting either a context
        variable of a context URI.
        """
        def doIt(node, parent, sseellff):
            if node.is_spo and len(node.arguments) == 3:
                contexts = sseellff.contexts
                if len(contexts) > 1:
                    node.arguments.append(self.get_fresh_variable())
                else:
                    node.arguments.append(Term(Term.RESOURCE, contexts[0]))
        self.walk(doIt, types=OpExpression, external_value=self)

    ###########################################################################################################
    ## SPARQL-specific
    ###########################################################################################################
    
    def color_filter_nodes(self):
        """
        Color operator node as a filter if it is a comparison, or if all of its
        children are filters.
        Hack: Or if its a 'bound' predicate.
        TODO: CONVERT bound HACK INTO GENERIC TEST
        """
        def colorIt(node, parent, external_value):
            node.color = None
            if isinstance(node, Term): return
            if node.operator in COMPARISON_OPERATORS:
                node.color = 'FILTER'
            elif node.predicate == 'bound' and len(node.arguments) == 1:
                node.color = 'FILTER'
            else:
                for arg in node.arguments:
                    if not arg.color == 'FILTER': return
                node.color = 'FILTER'
        ## color some filter nodes:
        self.walk(colorIt, bottom_up=True)
    
    def convert_predications_to_spo_nodes(self):
        """
        Convert all predications to SPO nodes.
        Extract contexts from arguments an insert them into the 'context' attribute
        """
        def doit(node, parent, external_value):
            if node.operator == OpExpression.PREDICATION and node.predicate and not node.is_spo:
                predicate = node.predicate
                subject = node.arguments[0]
                object = node.arguments[1]
                node.context = node.arguments[2] if len(node.arguments) == 3 else None
                node.predicate = 'QUAD' if node.context else 'TRIPLE'
                node.arguments = [subject, predicate, object]
                node.is_spo = True
            elif node.is_spo and len(node.arguments) == 4:
                node.context = node.arguments[3]
                node.arguments.remove(node.context)
        self.walk(doit, types=OpExpression)
 
    def bubble_up_contexts(self):
        """
        Locate quad nodes, trim their length from 4 to 3, and propagate the contexts
        they reference to their ancestors.        
        """
        ## migrate contexts out of triple nodes, and inherit them up where possible
        def bubble_up(node, parent, external_value):
            if not node.arguments: return
            context = None
            for arg in node.arguments:
                if isinstance(arg, Term) or not arg.context or not arg.context.term_type == Term.VARIABLE: return
                if not context: context = arg.context
                elif not arg.context.value == context.value: return
            ## if we reach here, there is a context common to all children of 'node":
            node.context = context
            for arg in node.arguments:
                arg.context = None
        self.walk(bubble_up, types=OpExpression, bottom_up=True)
        def bubble_back_down(node, parent, external_value):
            if node.context and node.operator == OpExpression.OPTIONAL:
                node.arguments[0].context = node.context
                node.context = None
        ## some nodes (optionals) shouldn't have context attached:
        self.walk(bubble_back_down, types=OpExpression)
        def create_graph_node(node, parent, sseellff):
            if node.context and not node.is_spo:
                graphNode = OpExpression('GRAPH', [node])
                graphNode.context = node.context
                sseellff.substitute_node(node, graphNode)                
        ## create a graph node for each node that has a context but is not a triple node
        self.walk(create_graph_node, types=OpExpression, bottom_up=True, external_value=self)

    def contextify_sparql_triples(self):
        """
        Insure that a GRAPH declaration is wrapped around each triple,
        so that a CONTEXTS filter can apply everywhere.
        """
        contextified = set([])
        uncontextified = set([])
        def mark_contextified_nodes(node, parent, external_value):
            if node.context or parent in contextified: contextified.add(node)
        def collect_uncontextified(node, parent, external_value):
            if not node.context and not node in contextified:
                uncontextified.add(node)
        ## 'mark' all variables at or below a context
        self.walk(mark_contextified_nodes, types=OpExpression)
        ## collect list of triple nodes that are not 'marked'
        self.walk(collect_uncontextified, types=OpExpression)
        ## add a fresh context variable to each unmarked triple node:
        for node in uncontextified:
            node.context = self.get_fresh_variable()
        
    def fix_heterogeneous_disjunctions(self):
        """
        A query that disjoins a triple clause with a filter clause cannot produce SPARQL
        output.  Apply deMorgan's to distribute the disjunction, so that a more complex,
        less performant query is created that CAN produce SPARQL code.
        """
        self.color_filter_nodes()
        heteroDisjuncts = []
        def collect_hetero_disjuncts(node, parent, external_value):
            if not node.operator == OpExpression.OR: return
            if not isinstance(parent, OpExpression) or not parent.operator == OpExpression.AND: return
            filter = False
            triple = False
            for arg in node.arguments:
                if arg.color == 'FILTER': filter = True
                else: triple = True
            if filter and triple: heteroDisjuncts.append(node)
        self.walk(collect_hetero_disjuncts, types=OpExpression)
        #print "FOUND HETEROS", heteroDisjuncts
        if heteroDisjuncts:
            self.distribute_disjunction(heteroDisjuncts[0])
            ## to be safe, we fix only one hetero disjunct at a time, and then recurse:
            self.fix_heterogeneous_disjunctions()
            
    def denormalize_leading_filter(self, leader):
        """
        Called by 'denormalize_filter_ands' to denormalize
        the filters beginning with 'leader'
        """
        parent = leader.parent
        predecessors = []
        filters = []
        successors = []
        for arg in parent.arguments:
            if arg == leader: filters.append(arg)
            elif successors: successors.append(arg)
            elif filters:
                if arg.color == 'FILTER': filters.append(arg)
                else: successors.append(arg)
            else: predecessors.append(arg)
        ## we now have consecutive filters:
        if len(filters) < 2: raise Exception("Bug in 'denormalize_filter_ands'")
        newAnd = OpExpression(OpExpression.AND, filters)
        predecessors.append(newAnd)
        predecessors.extend(successors)
        parent.arguments = predecessors
        self.recompute_backlinks(where_clause_only=True)
    
    def denormalize_filter_ands(self):
        """
        Filter and triple nodes get AND'ed together.  Separate out consecutive AND'ed filter nodes
        into their own AND node, so that the filter can be printed easily.
        """
        self.color_filter_nodes()
        leaders = []
        def collect_some_leading_filter_ands(node, parent, external_value):
            """
            If we find two consecutive AND'd filter nodes, collect the first. 
            """
            if not node.operator == OpExpression.AND: return
            siblingFilters = []
            for arg in node.arguments:
                if arg.color == 'FILTER':
                    siblingFilters.append(arg)
                elif siblingFilters:
                    ## found non filter, time to exit
                    if len(siblingFilters) > 1: leaders.append(siblingFilters[0])
                    return
            ## guard against case when ALL of the children are filters:
            if len(siblingFilters) > 1 and len(siblingFilters) < len(node.arguments): leaders.append(siblingFilters[0])                    
        ## collect some of the leading filters
        self.walk(collect_some_leading_filter_ands, types=OpExpression)
        if leaders:
            self.denormalize_leading_filter(leaders[0])
            ## recurse
            self.denormalize_filter_ands()
            
    def translate_negations(self):
        """
        Translate negation into optional and unbound.
        Assumes that normalization guarantees that the argument
        to a negation is always a predication (not yet implemented).
        (not (triple S P O)) ::=
             (and (optional (and (triple S P ?v1) (= ?v1 O)))
                  (not (bound ?v1)))
        Optimization:  If the 'O' argument is a variable that appears nowhere else
        in the WHERE clause, then we can omit the extra variable and the equality clause.
        """
        didIt = [False]
        def doit(node, parent, sseellff):
            if not node.operator == OpExpression.NOT: return
            tripleNode = node.arguments[0]
            if not (tripleNode.operator == OpExpression.PREDICATION and tripleNode.is_spo): return
            objArg = tripleNode.arguments[2]
            isSimple = (objArg.term_type == Term.VARIABLE and 
                        True) #sseellff.is_unique_variable_within_where_clause(objArg))
            if isSimple:
                optionalNode = OpExpression(OpExpression.OPTIONAL, [tripleNode])
                boundNode =  OpExpression(OpExpression.PREDICATION, [objArg])
                boundNode.predicate = 'bound'
            else:
                freshVar = sseellff.get_fresh_variable()
                tripleNode.arguments[2] = freshVar
                equalityNode = OpExpression(OpExpression.EQUALITY, [freshVar, objArg])
                innerAndNode = OpExpression(OpExpression.AND, [tripleNode, equalityNode])
                optionalNode = OpExpression(OpExpression.OPTIONAL, [innerAndNode])
                boundNode =  OpExpression(OpExpression.PREDICATION, [freshVar])
                boundNode.predicate = 'bound'
            notNode = OpExpression(OpExpression.NOT, [boundNode])
            outerAndNode = OpExpression(OpExpression.AND, [optionalNode, notNode])
            sseellff.substitute_node(node, outerAndNode)
            sseellff.recompute_backlinks()
            didIt[0] = True
        self.walk(doit, types=OpExpression, external_value=self)
        if didIt[0]:
            self.flatten_nested_ands()
     

    ###########################################################################################################
    ## Language-specific Normalization Scripts
    ###########################################################################################################
    
    def normalize_for_prolog(self):
        self.propagate_constants_to_predications()
        if self.contexts:
            self.quadify_triples()            
            self.filter_quad_contexts()
        self.translate_optionals(p_or_true=False)
        self.flatten_select_terms()
        self.flatten_value_computations()
        ## TEMPORARY TO SEE WHAT IT LOOKS LIKE:
        #self.translate_in_enumerate_into_disjunction_of_equalities()
        ## END TEMPORARY

    def normalize_for_sparql(self):
        """
        Reorganize the parse tree to be compatible with SPARQL's bizarre syntax.
        """
        self.propagate_constants_to_predications(skip_context_variables=True)
        self.convert_predications_to_spo_nodes()
        self.fix_heterogeneous_disjunctions()
        if False: ## too slow:
            self.translate_in_enumerate_into_disjunction_of_equalities()
        else:
            self.translate_in_enumerate_into_temporary_join()
        #ps("CCC", self.parse_tree)                
        self.bubble_up_contexts()
        #ps("DDD", self.parse_tree)
        if self.contexts:                        
            self.contextify_sparql_triples()
        self.translate_negations()
        ## finally, create non-normalized structure to assist filters output
        self.denormalize_filter_ands()
        self.flatten_nested_ands()
        self.color_filter_nodes()

def pc(msg, parse_tree):
    print msg, str(StringsBuffer(complain='SILENT').common_logify(parse_tree))

def ps(msg, parse_tree):
    print msg, str(StringsBuffer(complain='SILENT').sparqlify(parse_tree))

###########################################################################################################
##
###########################################################################################################

class StringsBuffer:
    NEWLINE = '\n'
    def __init__(self, include_newlines=False, complain=None, spoify_output=False, infix_with_prefix_triples=False):
        self.buffer = []
        self.include_newlines = include_newlines
        self.running_indent = 0
        self.complain_flag = complain
        self.execution_language = None
        self.infix_with_prefix_triples = infix_with_prefix_triples  # infix CommonLogic hack
        self.spoify_output = spoify_output  # Prolog hack
    
    def append(self, item):
        if item is None:
            print "BREAK"
        self.buffer.append(item)
        return self
    
    def pop(self):
        """
        Remove the last item/string in the buffer.
        """
        self.buffer.pop()
        return self
        
    def newline(self):
        if self.include_newlines:
            self.append('\n')
        else:
            self.append(' ')
        return self
            
    def delimiter(self, delimiter):
        if not self.include_newlines:
            delimiter = delimiter.replace('\n', ' ')
        self.append(delimiter)
        return self
    
    def indent(self, indent):
        self.append(indent)
        self.running_indent = indent
        return self
    
    def process_embedded_indents(self):
        strings = []
        indent = 0
        for s in self.buffer:
            if isinstance(s, int):
                indent = s
            else:
                s = s.replace('\n', '\n' + ' ' * indent)
                strings.append(s)
        self.buffer = strings            
    
    def stringify(self):
        self.process_embedded_indents()        
        return ''.join(self.buffer)
    
    def __str__(self):
        return self.stringify()
    
    def complain(self, term, operator):
        """
        'term' represents an operator not implemented in the target language.
        Raise an exception, or append highly visible syntax indicating the problem.
        TODO: RAISE EXCEPTION
        """
        if self.complain_flag == 'SILENT':
            self.append(operator + "!!! ")
            return self
        else:
            type = term.term_type if isinstance(term, Term) else term.operator if isinstance(term, OpExpression) else operator
            raise QueryMissingFeatureException("%s is unable to evaluate an expression of type %s" % (self.execution_language, type))            
    
    def common_logify(self, term, brackets=None, delimiter=' '):
        if isinstance(term, Term):
            self.append(str(term))
        elif isinstance(term, str):
            self.append(term)
        elif isinstance(term, list):        
            if brackets: self.append(brackets[0])
            for arg in term:
                self.common_logify(arg)
                self.delimiter(delimiter)
            if term:
                self.pop()  ## pop trailing delimiter
            if brackets: self.append(brackets[1])
        elif isinstance(term, QueryBlock):
            self.append('(select ')
            if term.distinct: self.append('distinct ')
            self.append('(').common_logify(term.select_terms, delimiter=' ').append(')\n')
            self.append(' where ')
            self.indent(7).common_logify(term.where_clause)
            if term.contexts_clause:
                self.indent(1).append('\ncontexts (').common_logify(term.contexts_clause, delimiter=' ').append(')')
            if term.limit >= 0:
                self.indent(1).append('\nlimit ' + str(term.limit))
            self.append(')')
        elif isinstance(term, OpExpression):
            if term.operator == OpExpression.ENUMERATION:
                self.append('(')
                self.common_logify(term.predicate.lower()).append(' ')
                self.common_logify(term.arguments)
                self.append(')') 
            elif term.operator in [OpExpression.AND, OpExpression.OR]:
                self.append('(')
                self.append(term.operator.lower()).append(' ')
                self.common_logify(term.arguments, delimiter='\n')
                self.append(')')
            else:
                self.append('(')
                if term.is_spo: term.predicate = term.predicate.lower()
                self.common_logify(term.predicate or term.operator.lower()).append(' ')
                self.common_logify(term.arguments)
                self.append(')') 
        return self

    def infix_common_logify(self, term, brackets=None, delimiter=' ', suppress_parentheses=False):
        if isinstance(term, Term):
            self.append(str(term))
        elif isinstance(term, str):
            self.append(term)
        elif isinstance(term, list):        
            if brackets: self.append(brackets[0])
            for arg in term:
                self.infix_common_logify(arg)
                self.delimiter(delimiter)
            if term:
                self.pop()  ## pop trailing delimiter
            if brackets: self.append(brackets[1])
        elif isinstance(term, QueryBlock):
            self.append('select ')
            if term.distinct: self.append('distinct ')            
            self.infix_common_logify(term.select_terms, delimiter=' ').newline()
            self.append('where ')
            self.indent(6).infix_common_logify(term.where_clause, suppress_parentheses=True)
            if term.contexts_clause:
                self.indent(0).append('\ncontexts ').common_logify(term.contexts_clause, delimiter=' ')
            if term.limit >= 0:
                self.indent(0).append('\nlimit ' + str(term.limit))                
        elif isinstance(term, OpExpression):
            if term.operator == OpExpression.ENUMERATION:
                self.infix_common_logify(term.arguments, brackets=('[', ']'), delimiter=', ')
            elif term.operator in [OpExpression.AND, OpExpression.OR]:                
                brackets = ('(', ')') if not suppress_parentheses else None
                self.infix_common_logify(term.arguments, delimiter='\n%s ' % term.operator.lower(), brackets=brackets)
            elif term.operator in [OpExpression.NOT, OpExpression.OPTIONAL]:
                if self.infix_with_prefix_triples:
                    self.append(term.operator.lower()).append('(').infix_common_logify(term.arguments[0]).append(')')
                else:
                    if not suppress_parentheses: self.append('(')
                    self.append(term.operator.lower() + ' ').infix_common_logify(term.arguments[0])
                    if not suppress_parentheses: self.append(')')                              
            elif term.operator in CONNECTIVE_OPERATORS:
                self.append('(')                
                self.infix_common_logify(term.arguments, delimiter=' %s ' % term.operator.lower())
                self.append(')')
            elif term.predicate and self.infix_with_prefix_triples:
                if term.is_spo: term.predicate = term.predicate.lower()
                self.infix_common_logify(term.predicate)
                self.infix_common_logify(term.arguments, brackets=('(', ')'))
            else:
                self.append('(')
                if term.is_spo: term.predicate = term.predicate.lower()
                self.infix_common_logify(term.predicate or term.operator.lower()).append(' ')
                self.infix_common_logify(term.arguments)
                self.append(')')     
        return self
    
#    LISPP_HACK_COUNTER = [0]
    
    def prologify(self, term, brackets=None, delimiter=' ', suppress_parentheses=False, spoify_output=True):
#        def hack_variable():
#            StringsBuffer.LISPP_HACK_COUNTER[0] = StringsBuffer.LISPP_HACK_COUNTER[0] + 1
#            return '?hack' + str(StringsBuffer.LISPP_HACK_COUNTER[0])
        if isinstance(term, Term):
            if term.term_type == Term.RESOURCE:
                self.append('!').append(str(term))
            elif term.term_type == Term.LITERAL:
                self.append('!').append(str(term))
            else: ## variable, I guess
                self.append(str(term))
        elif isinstance(term, str):
            self.append(term)
        elif isinstance(term, list):        
            if brackets: self.append(brackets[0])
            for arg in term:
                self.prologify(arg)
                self.delimiter(delimiter)
            if term:
                self.pop()  ## pop trailing delimiter
            if brackets: self.append(brackets[1])
        elif isinstance(term, QueryBlock):
            self.execution_language = CommonLogicTranslator.PROLOG
            self.spoify_output = spoify_output
            if term.distinct:
                self.append('(select-distinct ')
            else:
                self.append('(select ')        
            self.indent(8).prologify(term.select_terms, brackets=('(', ')'), delimiter=' ').newline()   
            self.prologify(term.where_clause, suppress_parentheses=True)
            if term.limit >= 0:
                self.append('\n(:limit ' + str(term.limit) + ')')
            self.append(')')
        elif isinstance(term, OpExpression):
            if term.operator == OpExpression.ENUMERATION:
                self.append('(?? (list ')
                self.prologify(term.arguments, delimiter=' ')
                self.append('))')  
            elif term.operator == OpExpression.IN:
                self.append('(member ').prologify(term.arguments, delimiter=' ').append(')')
            elif term.operator == OpExpression.EQUALITY:
                ## TEMPORARY UNTIL PROLOG IS FIXED
                self.append('(lispp (upi= ').prologify(term.arguments, delimiter=' ').append('))')
            elif term.operator == OpExpression.AND:
                if suppress_parentheses:
                    self.prologify(term.arguments, delimiter='\n')
                else:
                    self.append('(and ').prologify(term.arguments, delimiter='\n').append(')')
            elif term.operator in COMPARISON_OPERATORS and not term.operator == '=':
                op = term.operator.lower()
                ## EXPERIMENT
                op = 'cl:' + op
                ## END EXPERIMENT
                self.append('(lispp (').append(op).append(' ').prologify(term.arguments[0]).append(' ')
                self.prologify(term.arguments[1]).append('))')
            elif term.operator == OpExpression.TRUE:
                self.append('(lispp t)')
            elif term.is_spo:
                self.append('(q ')
                self.prologify(term.arguments, delimiter=' ')
                self.append(')')
            elif term.predicate and self.spoify_output:
                self.append('(q')
                self.append(' ').prologify(term.arguments[0]).append(' ').prologify(term.predicate)
                self.append(' ').prologify(term.arguments[1])
                if len(term.arguments) > 3:
                    self.append(' ').prologify(term.arguments[2])
                self.append(')')
            elif term.operator in [OpExpression.OPTIONAL]:
                self.complain(term, 'OPTIONAL')
            elif term.operator == OpExpression.COMPUTE:
                self.append('(lisp ').prologify(term.arguments[0]).append(' (')
                self.prologify(term.arguments[1:], delimiter=' ').append('))')
            else:
                self.append('(')
                self.prologify(term.predicate or term.operator.lower()).append(' ')
                self.prologify(term.arguments, delimiter=' ')
                self.append(')')  
        return self

    def sparqlify(self, term, brackets=None, delimiter=' ', suppress_curlies=False, suppress_filter=False):
        if isinstance(term, Term):
            self.append(str(term))
        elif isinstance(term, str):
            self.append(term)
        elif isinstance(term, list):        
            if brackets: self.append(brackets[0])
            for arg in term:
                self.sparqlify(arg, suppress_filter=suppress_filter)
                self.delimiter(delimiter)
            if term:
                self.pop()  ## pop trailing delimiter
            if brackets: self.append(brackets[1])
        elif isinstance(term, QueryBlock):
            self.execution_language = CommonLogicTranslator.SPARQL
            self.append('select ')    
            if term.distinct: self.append('distinct ')                 
            self.sparqlify(term.select_terms, delimiter=' ').newline()   
            self.append('where ').append('{ ')
            self.indent(6).sparqlify(term.where_clause, suppress_curlies=True)
            self.append(' }')
            if term.limit >= 0:
                self.indent(0).append('\nlimit ' + str(term.limit))                                
        elif isinstance(term, OpExpression):
            if term.operator in [OpExpression.AND, OpExpression.OR]:
                ## are we joining triples or filters? look at first non-connective in descendants
                ## to find out:
#                sampleArg = term.arguments[0]
#                while sampleArg.operator in [OpExpression.AND, OpExpression.OR]:
#                    sampleArg = sampleArg.arguments[0]
#                if sampleArg.predicate:
#                    brackets = ('{ ', ' }') if not suppress_curlies else None
#                    delimiter = ' .\n' if term.operator == OpExpression.AND else '\nunion '
#                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter)     
#                else:
#                    if not suppress_filter: self.append('filter ')
#                    brackets = ('(', ')')
#                    delimiter = ' && ' if term.operator == OpExpression.AND else ' || '
#                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter, suppress_filter=True)
                if term.color == 'FILTER':
                    if not suppress_filter: self.append('filter ')
                    brackets = ('(', ')')
                    delimiter = ' && ' if term.operator == OpExpression.AND else ' || '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter, suppress_filter=True)                
                else:
                    brackets = ('{ ', ' }') if not suppress_curlies else None
                    delimiter = ' .\n' if term.operator == OpExpression.AND else '\nunion '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter)     
            elif term.operator == OpExpression.OPTIONAL:
                #if not suppress_curlies: self.append('{')              
                self.append('optional ').sparqlify(term.arguments[0])
                #if not suppress_curlies: self.append('}')      
            elif term.operator == OpExpression.NOT:
                if not term.color == 'FILTER': self.complain(term, "NOT") ## eventually shouldn't occur
                if not suppress_filter: self.append('filter ')
                self.append('(')
                self.append('!').sparqlify(term.arguments[0])
                self.append(')')
            elif term.is_spo:
                if not suppress_curlies: self.append('{')                
                if term.context: self.append('graph ').sparqlify(term.context).append(' { ')
                self.sparqlify(term.arguments, delimiter=' ')
                if term.context: self.append(' } ')
                if not suppress_curlies: self.append('}') 
            elif term.predicate:
                ## TEMPORARY HACK.  TODO: MAKE IT GENERIC:
                if term.predicate == 'bound':
                    self.append(term.predicate + '(').sparqlify(term.arguments[0]).append(')')
                elif True:
                    raise Exception("SPARQL normalization failed to eliminate non-spo predication")
                else:
                    if not suppress_curlies: self.append('{')
                    self.sparqlify(term.arguments[0]).sparqlify(' ').sparqlify(term.predicate).sparqlify(' ')
                    self.sparqlify(term.arguments[1]).sparqlify(' ')
                    if not suppress_curlies: self.append('}')                
            elif term.operator in COMPARISON_OPERATORS:
                if not suppress_filter: self.append('filter ')
                self.append('(')
                self.sparqlify(term.arguments[0]).sparqlify(' ').sparqlify(term.operator).sparqlify(' ')
                self.sparqlify(term.arguments[1]).sparqlify(' ')
                self.append(')')
            elif term.operator in PREFIX_OPERATORS:
                ## TODO: ADD TRANSLATION HERE FROM CL FUNCTORS TO SPARQL FUNCTORS
                ## RIGHT NOW 'REGEX' IS THE ONLY ONE:
                functor = term.operator
                self.append('filter ' + functor + '(').sparqlify(term.arguments, delimiter=', ').append(')')                  
            elif term.operator == 'GRAPH':
                if not suppress_curlies: self.append('{')                
                self.append('graph ').sparqlify(term.context).append(' { ').sparqlify(term.arguments[0], suppress_curlies=True).append(' } ')
                if not suppress_curlies: self.append('}') 
        return self


###########################################################################################################
## 
###########################################################################################################

def translate_common_logic_query(query, preferred_language='PROLOG', contexts=None, complain='EXCEPTION',
                                 subject_comes_first=False):
    """
    Translate a Common Logic query into either SPARQL or PROLOG syntax.  If 'preferred_language,
    choose that one (first).  Return three
    values, the query, either 'SPARQL' or 'PROLOG', and an error message if the
    translation fails.  It may fail either because the syntax is illegal, or because
    the combination of expressions in the query is not implementable in either SPARQL
    or PROLOG
    """
    def help_translate(language):
        trans = CommonLogicTranslator(query, subject_comes_first=subject_comes_first)
        trans.parse()
        Normalizer(trans.parse_tree, language, contexts).normalize()
        if language == CommonLogicTranslator.PROLOG:
            translation = str(StringsBuffer(complain=complain, spoify_output=True).prologify(trans.parse_tree))
        elif language == CommonLogicTranslator.SPARQL:
            translation = str(StringsBuffer(complain=complain).sparqlify(trans.parse_tree))
        else:
            raise IllegalOptionException("No translation available for the execution language '{0}'".format(language))
        return translation, trans.parse_tree.contexts_clause, trans.parse_tree.temporary_enumerations
    
    try:
        preferred_language = preferred_language or 'PROLOG'
        translation, contexts, temporary_enumerations = help_translate(preferred_language)
        successfulLanguage = preferred_language
    except QueryMissingFeatureException, e1:
        try:
            otherLanguage = 'SPARQL' if preferred_language == 'PROLOG' else 'PROLOG'
            translation, contexts, temporary_enumerations = help_translate(otherLanguage)
            successfulLanguage = otherLanguage
        except QueryMissingFeatureException:
            return None, None, None, e1
    return translation, contexts, temporary_enumerations, successfulLanguage, None

def contexts_to_uris(context_terms, repository_connection):
    """
    Convert the URIs and qnames in 'context_terms' into URIs 
    BUG: ASSUMES THAT ANY QNAMES REFERENCE LOCALLY-DECLARED PREFIXES, I.E.
    DOESN'T WORK FOR PREFIXES REGISTERED SERVER-SIDE
    """
    contexts = []
    for cxt in context_terms:
        if cxt.qname:
            prefix, localName = cxt.qname.split(':')
            ns = repository_connection.getNamespace(prefix)
            if not ns:
                raise Exception("Can't find a namespace for the prefix '%s' in the contexts reference '%s'" % (prefix, cxt))
            contexts.append("<{0}>".format(ns + localName))
        else:
            contexts.append(str(cxt))
    return [repository_connection.createURI(cxt) for cxt in contexts]
    

###########################################################################################################
## Testing
###########################################################################################################


def translate(cl_select_query, target_dialect=CommonLogicTranslator.PROLOG, contexts=None):
    trans = CommonLogicTranslator(cl_select_query)
    trans.parse()
    print "\nCOMMON LOGIC \n" + str(StringsBuffer(include_newlines=True, complain='SILENT').common_logify(trans.parse_tree))    
    print "\nINFIX COMMON LOGIC \n" + str(StringsBuffer(include_newlines=True, complain='SILENT', infix_with_prefix_triples=trans.infix_with_prefix_triples).infix_common_logify(trans.parse_tree))
    Normalizer(trans.parse_tree, CommonLogicTranslator.SPARQL).normalize()        
    print "\nSPARQL \n" + str(StringsBuffer(include_newlines=True, complain='SILENT').sparqlify(trans.parse_tree))
    trans = CommonLogicTranslator(cl_select_query)
    trans.parse()    
    Normalizer(trans.parse_tree, CommonLogicTranslator.PROLOG, contexts=contexts).normalize()
    print "\nPROLOG \n" + str(StringsBuffer(include_newlines=True, complain='SILENT', spoify_output=True).prologify(trans.parse_tree))    


query1 = """(select (?s ?o) where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>))"""
query1i = """select ?s ?o where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)"""
query2 = """(select (?s ?o) where (and (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)))"""
query2i = """select ?s ?o where ex:name(?s ?o) and rdf:type(?s <http://www.franz.com/example#Person>)"""
query3 = """(select (?s ?o) where (and (ex:name ?s ?o) (= ?o "Fred")))"""
query3i = """select ?s ?o where (ex:name ?s ?o) and (?o = "Fred")"""
query4 = """(select (?s ?o) where (and (or (ex:name ?s ?o) (ex:title ?s ?o)) (= ?o "Fred")))"""
query4i = """select ?s ?o where ((ex:name ?s ?o) or (ex:title ?s ?o))and (?o = "Fred")"""
query5 = """(select (?s) where (and (ex:name ?s ?o) (or (= ?o "Fred") (= ?o "Joe"))))"""
query5i = """select ?s where (ex:name ?s ?o) and ((?o = "Fred") or (?o = "Joe"))"""
query6 = """(select (?s ?o) where (and (ex:name ?s ?o) (not (rdf:type ?s <http://www.franz.com/example#Person>))))"""
query6i = """select ?s ?o where (ex:name ?s ?o) and not (rdf:type ?s <http://www.franz.com/example#Person>)"""
query7 = """(select (?s ?o) where (and (triple ?s ex:name ?o) (triple ?s rdf:type <http://www.franz.com/example#Person>)))"""
query7i = """select ?s ?o where triple(?s ex:name ?o) and triple(?s rdf:type <http://www.franz.com/example#Person>)"""
query8 = """(select (?s ?o) where (and (quad ?s ex:name ?o ?c) (= ?c ex:cxt)))"""
query8i = """select ?s ?o where quad(?s ex:name ?o ?c) and (?c = ex:cxt)"""
query9 = """(select (?s ?age) where (and (ex:age ?s ?age) (>= ?age 21)))"""
query9i = """select ?s ?age where ex:age(?s ?age) and (?age >= 21)"""
query10 = """(select (?name ?age) where (and (triple ?s rdf:type ex:Person) (optional (triple ?s ex:name ?name)) 
                                             (optional (and (ex:age ?s ?age) (> ?age 21)))))"""
query10i = """select ?name ?age where triple(?s rdf:type ex:Person) and optional (triple ?s ex:name ?name) 
                                       and optional (and ex:age(?s ?age) (?age > 21))"""
query11 = """(select (?s ?o) where (and (((ex:name ?s ?o))) (((triple ?s rdfs:label ?o)))))"""
query11i = """select ?s ?o where ((ex:name(?s ?o))) and ((triple(?s rdfs:label ?o)))"""  ## triple part screws up
query12 = """(select (?name) where (and (rdf:type ?c ex:Company) (ex:gross ?c ?gross) (ex:expenses ?c ?expenses)
                                                (> (- ?gross ?expenses) 50000) (ex:name ?c ?name)))"""
query12i = """select ?name where rdf:type(?c ex:Company) and ex:gross(?c ?gross) and ex:expenses(?c ?expenses)
                                                and ((?gross - ?expenses) > 5000) and ex:name(?c ?name)"""
query13 = """(select (?s ?p ?o) where (in ?s (list <http://foo> <http://bar>)) (triple ?s ?p ?o))"""
query13i = """select ?s ?p ?o where triple(?s ?p ?o) and ?s in [<http://foo> <http://bar>]"""   
query14 = """(select distinct (?s ?p ?o) where (triple ?s ?p ?o) contexts (ex:context1 ex:context2) limit 5)"""                                            
query14i = """select distinct ?s ?p ?o where triple(?s ?p ?o) contexts ex:context1, ex:context2 limit 5"""    
query15 = """(select (?s) where (and (or (ex:p1 ?s1 ?o1 ?c1) (ex:p2 ?s2 ?o2 ?c1)  (ex:p3 ?s3 ?o3 ?c2))
                                     (or (ex:p4 ?s4 ?o4 ?c1) (ex:p5 ?s5 ?o5 ?c1))))"""
query16 = """(select (?s ?p ?o ?c ?p2 ?o2 )  where (and (quad ?s ?p ?o ?c) (optional (quad ?o ?p2 ?o2 )))"""
query16i = """select ?s ?o ?c ?o2 ?c2 where quad(?s ex:p ?o ?c)  and optional(quad(?o ex:p2 ?o2 ?c2))"""
query17 = """(select (?s) where (triple ?s ?p ?o) (= ?s ex:Bill))"""

query18 = """(select (?s ?o) where (or (triple ?s foaf:name ?o)
                        (and (not (triple ?s foaf:name ?o1))
                             (or (triple ?s foaf:mbox ?o)
                                 (not (triple ?s foaf:mbox ?o2))))))"""
                                 
query19i = """select ?o ?lac ?otype ?c2
where (?o in [ex:foo, ex:bar]) and
       ( triple(?o <%s> ?lac) or
        quad(?o rdf:type ?otype ?c2) )"""
        
query19i = """(select (?s)
where (or (triple ?wall <http://www.wildsemantics.com/systemworld#gridWidgets> ?s)
          (and (triple ?wall <http://www.wildsemantics.com/systemworld#gridWidgets> ?widget1)
               (or (triple ?widget1 <http://www.wildsemantics.com/systemworld#backingTopic> ?s)
                   (triple ?widget1 <http://www.wildsemantics.com/systemworld#filterSet> ?s)))
          (and (triple ?wall <http://www.wildsemantics.com/systemworld#freeWidgets> ?widget2)
               (or (triple ?widget2 <http://www.wildsemantics.com/systemworld#backingTopic> ?s)
                   (triple ?widget2 <http://www.wildsemantics.com/systemworld#filterSet> ?s)))))

    """
                                       



query20i = """select ?s ?p ?o ?c ?lac ?c2
where (?c ?s ?p ?o) 
  and optional (?c2 ?o <http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac)
  and ((?s = ?wall)
    or ((?wall <http://www.wildsemantics.com/systemworld#gridWidgets> ?widget1)
      and ((?s = ?widget1)
        or (?widget1 <http://www.wildsemantics.com/systemworld#backingTopic> ?s)
        or (?widget1 <http://www.wildsemantics.com/systemworld#filterSet> ?s)))
    or ((?wall <http://www.wildsemantics.com/systemworld#freeWidgets> ?widget2)
      and ((?s = ?widget2)
        or (?widget2 <http://www.wildsemantics.com/systemworld#backingTopic> ?s)
        or (?widget2 <http://www.wildsemantics.com/systemworld#filterSet> ?s)))) 
"""

query20i = """select ?s ?p ?o ?c ?lac ?otype ?c2  where quad(?s ?p ?o ?c)  and 
      (optional quad(?o <http://fiz> ?lac ?c2)) """
      
query20i = """select ?s ?p ?o ?c ?lac ?otype ?c2 where (?c ?s ?p ?o)  and 
      (?s in [<http://www.wildsemantics.com/worldworld#SystemWorld_World>, <http://www.wildsemantics.com/worldworld#WorldWorld_World>, <http://www.wildsemantics.com/worldworld#AuthWorld_World>, <http://www.wildsemantics.com/worldworld#GardenWorld_World>, <http://www.wildsemantics.com/worldworld#VocabWorld_World>, <http://www.wildsemantics.com/worldworld#PermissionsWorld_World>, <http://www.wildsemantics.com/worldworld#PublicWorld_World>])  and
      (optional quad(?o <http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac ?c2)) and
      (optional quad(?o rdf:type ?otype ?c2))"""

query20i = """select ?s ?p ?o ?c ?lac ?otype ?c2 
where  ((?cls = ?s)
       or (?cls <http://www.wildsemantics.com/systemworld#fields> ?s))  
  and quad(?s ?p ?o ?c) 
  and optional (quad(?o <http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac ?c2))
  and optional (quad(?o rdf:type ?otype ?c2))
"""

query20i = """select ?o ?lac ?otype ?c2
where (?o in [http://www.wildsemantics.com/systemworld#World]) and
       ( triple(?o <http://www.wildsemantics.com/systemworld#lookAheadCapsule> ?lac) or
        quad(?o rdf:type ?otype ?c2) )

"""

query20 = """
 
"""


if __name__ == '__main__':
    switch = 20
    print "Running test", switch
    if switch == 1: translate(query1)  # IMPLICIT AND
    elif switch == 1.1: translate(query1i)
    elif switch == 2: translate(query2) # EXPLICIT AND
    elif switch == 2.1: translate(query2i)
    elif switch == 3: translate(query3) # EQUALITY
    elif switch == 3.1: translate(query3i)        
    elif switch == 4: translate(query4) # TRIPLE DISJUNCTION
    elif switch == 4.1: translate(query4i)        
    elif switch == 5: translate(query5) # FILTER DISJUNCTION
    elif switch == 5.1: translate(query5i)        
    elif switch == 6: translate(query6) # NEGATION
    elif switch == 6.1: translate(query6i)        
    elif switch == 7: translate(query7) # TRIPLE PREDICATE
    elif switch == 7.1: translate(query7i)        
    elif switch == 8: translate(query8) # CONTEXT.   SPARQL BREAKS; PROLOG QUESTIONABLE
    elif switch == 8.1: translate(query8i)        
    elif switch == 9: translate(query9) # COMPARISON
    elif switch == 9.1: translate(query9i)        
    elif switch == 10: translate(query10)  # OPTIONAL
    elif switch == 10.1: translate(query10i)        
    elif switch == 11: translate(query11)  # NESTED PARENS
    elif switch == 11.1: translate(query11i)        
    elif switch == 12: translate(query12)  # ARITHMETIC EXPRESSIONS
    elif switch == 12.1: translate(query12i)        
    elif switch == 13: translate(query13, contexts=["http:ex#cxt1", "http:ex#cxt2"])  # IN ENUMERATION
    elif switch == 13.1: translate(query13i)        
    elif switch == 14: translate(query14)  # DISTINCT, CONTEXTS, AND LIMIT
    elif switch == 14.1: translate(query14i)        
    elif switch == 15: translate(query15)  # SPARQL GRAPH NODES
    #elif switch == 15.1: translate(query15i)    
    elif switch == 16: translate(query16, contexts=["http:ex#cxt1", "http:ex#cxt2"])  # BRACKETED OPTIONAL WITH QUAD (HARD FOR SOME REASON)
    elif switch == 16.1: translate(query16i)  
    elif switch == 17: translate(query17)    
    elif switch == 18: translate(query18) # NEGATION    
    elif switch == 19.1: translate(query19i)        
    elif switch == 20: translate(query20)                
    elif switch == 20.1: translate(query20i)            
    else:
        print "There is no test number %s" % switch

