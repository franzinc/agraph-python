#!/usr/bin/env python

from franz.openrdf.exceptions import *
from franz.openrdf.vocabulary.xmlschema import XMLSchema

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

class Token:
    VARIABLE = 'VARIABLE'
    STRING = 'STRING'
    QNAME = 'QNAME'
    URI = 'URI'
    BRACKET = 'BRACKET'
    RESERVED_WORD = 'RESERVED_WORD'
    NUMBER = 'NUMBER'
    BRACKET_SET = set(['(', ')', '[', ']',])
    RESERVED_WORD_SET = set(['AND', 'OR', 'NOT', 'OPTIONAL', 'IN', 'TRUE', 'FALSE', 'LIST',
                             '=', '<', '>', '<=', '>=', '!=', '+', '-', 'TRIPLE', 'QUAD',
                             'DISTINCT'])
    
    def __init__(self, token_type, value, offset=None):
        if token_type == Token.RESERVED_WORD:
            value = OpExpression.parse_operator(value)
        self.value = value
        self.token_type = token_type
        self.offset = offset
        
    def __str__(self):
        if self.token_type == Token.VARIABLE:
            return '?' + self.value
        elif self.token_type == Token.STRING:
            return '"%s"' % self.value
        else:
            return self.value
        
    @staticmethod
    def reserved_type(token, token_types):
        if not token: return False
        if isinstance(token, Term): return False
        if not token.token_type == Token.RESERVED_WORD: return False
        token_types = token_types if isinstance(token_types, (list, tuple, set)) else [token_types]
        for t in token_types:
            if token.value == t: return True
        return False

ATOMIC_TERM_TOKEN_TYPES = set([Token.VARIABLE, Token.STRING, Token.NUMBER, Token.URI, Token.QNAME])

LEGAL_VARIABLE_CHARS = set(['.', '_', '-',])

def grab_variable(string, tokens):
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
    tokens.append(token)
    return string[endPos:] if endPos < 9999 else ''

def grab_string(string, delimiter, tokens):
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
    tokens.append(token)
    return string[endPos + 1:]

def grab_uri(string, tokens):
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
    tokens.append(token)
    return string[endPos + 1:]

DELIMITER_CHARS = set([' ', ',', '(', ')', '[', ']'])

def grab_delimited_word(string, tokens):
    """
    The first token in 'string must be delimited by a blank,
    ## comma, or other delimiter.  Find the end, and convert the
    prefix in between into a token.  Or convert the entire thing
    into a token
    """
    endPos = -1
    for i in range(0, len(string)):
        c = string[i]
        if c in DELIMITER_CHARS:
            endPos = i
            break
    word = string[:endPos] if endPos >= 0 else string
    if word.upper() in Token.RESERVED_WORD_SET:
        tokens.append(Token(Token.RESERVED_WORD, word))
    elif word[0].isdigit():
        tokens.append(Token(Token.NUMBER, word))
    elif word.find(':') >=0:
        tokens.append(Token(Token.QNAME, word))
    else:
        raise QuerySyntaxException("Unrecognized term '%s'" % word)
    if endPos == -1:
        ## ran off the end of the string:
        return ''
    else:
        return string[endPos:]

def super_strip(string):
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

def tokenize(string, tokens, offset=0):
    """
    Parse 'string' into tokens.  Push tokens into 'tokens' during recursion,
    and return 'tokens'.
    """
    string = super_strip(string)
    if not string or string == ' ': return tokens
    c = string[0]
    if c == '?':
        suffix = grab_variable(string, tokens)
    elif c in Token.BRACKET_SET:
        tokens.append(Token(Token.BRACKET, c))
        suffix = string[1:]
    elif c in ['"', "'"]:
        suffix = grab_string(string, c, tokens)
    elif c == '<':
        suffix = grab_uri(string, tokens)
    ## at this point, the first token must be delimited by a blank,
    ## comma, or delimiter.  Find the end:
    else:
        #print "GRAB DELIMITED", string
        suffix = grab_delimited_word(string, tokens)
        #print "   SUFFIX '%s'" % suffix, "TOKEN", tokens[len(tokens) - 1]
    newToken = tokens[len(tokens) - 1]
    newToken.offset = offset
    return tokenize(suffix, tokens, offset=offset + len(string) - len(suffix))                   

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
        self.from_list = None
        self.where_clause = None
        self.distinct = False
        self.limit = -1
        
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
    def __init__(self, term_type, value, datatype=None, qname=None):
        self.term_type = term_type
        self.value = value
        self.datatype = datatype
        self.qname = qname
    
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

ARITHMETIC_OPERATORS = set(['+', '-'])
COMPARISON_OPERATORS = set(['=', '<', '>', '<=', '>=', '!='])
BOOLEAN_OPERATORS = set(['AND', 'OR', 'NOT', 'IN', 'OPTIONAL',]).union(COMPARISON_OPERATORS)
VALUE_OPERATORS = ARITHMETIC_OPERATORS
CONNECTIVE_OPERATORS = BOOLEAN_OPERATORS.union(VALUE_OPERATORS)
OPERATOR_EXPRESSIONS = CONNECTIVE_OPERATORS.union(ARITHMETIC_OPERATORS).union(set(['ENUMERATION', 'TRUE', 'FALSE', 'LIST', 'TRIPLE', 'QUAD'])) # omits 'PREDICATION'

class OpExpression:
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'
    IN = 'IN'
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
        self.is_ground = False
    
    @staticmethod
    def parse_operator(value):
        value = value.upper()
        if not value in OPERATOR_EXPRESSIONS and not value in ['DISTINCT']:
            raise Exception("Need to add '%s' to list of OPERATOR_EXPRESSIONS" % value)
        return value
        
    def __str__(self):
        return str(StringsBuffer().common_logify(self))

INFIX_WITH_PREFIX_TRIPLES = True

class CommonLogicTranslator:
    """
    """
    SPARQL = 'SPARQL'
    PROLOG = 'PROLOG'
    
    def __init__(self, query=None):
        self.set_source_query(query)
        self.parse_tree = None
        ## TEMPORARY PROLOG HACK
        self.groundify_all = False        
    
    def set_source_query(self, query):        
        self.source_query = query
        self.infix_parse = query and not query[0] == '('
        self.infix_with_prefix_triples = self.infix_parse and INFIX_WITH_PREFIX_TRIPLES
        
    SHIM = 3 ## hack until we figure out why offsets are not quite right
    
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
        else:
            raise Exception("Can't convert token %s to a term" % token)
        
    def parse_select_clause(self, select_string):
        """
        Parse 'select_string' and return a list of terms.
        TODO: UPGRADE TO ALLOW ARBITRARY EXPRESSIONS HERE
        """
        tokens = tokenize(select_string, [], len('select') - CommonLogicTranslator.SHIM)
        if tokens and Token.reserved_type(tokens[0], 'DISTINCT'):
            self.parse_tree.distinct = True
            tokens = tokens[1:]
        if not self.infix_parse:
            if len(tokens) >= 2 and tokens[0].value == '(' and tokens[len(tokens) - 1].value == ')':
                tokens = tokens[1:-1]
            else:
                self.syntax_exception("Missing parentheses around select clause arguments")
        terms = []
        for t in tokens:
            ## TODO: ALLOW ARBITRARY EXPRESSIONS HERE: 
#            if not t.token_type in [Token.URI, Token.STRING, Token.NUMBER, Token.VARIABLE, Token.QNAME]:
#                self.syntax_exception("Illegal operator '%s' found in select clause" % t.value, t)
            terms.append(self.token_to_term(t))
        return terms
        
    def parse_from_clause(self, from_string):
        """
        NOT YET IMPLEMENTED
        """
        return None        
    
    def parse_enumeration(self, value, tokens):
        arguments = self.parse_expressions(tokens, [])
        op = OpExpression(OpExpression.ENUMERATION, arguments)
        op.predicate = value
        return op
    
    def parse_predication(self, tokens, predicate=None):
        """
        'tokens' represent a predicate applied to arguments
        """
        if Token.reserved_type(predicate, [OpExpression.TRIPLE, OpExpression.QUAD]):
            predicate = predicate.value.lower()
            isGround = True
        ## I DON'T THINK WE DO THIS ANYMORE:
#        elif self.groundify_all:
#            isGround = True
#            predicate = OpExpression.TRIPLE.lower()
        else:
            isGround = False
        if predicate:
            arguments = self.parse_expressions(tokens, [])
        else:
            predicate = self.token_to_term(tokens[0])            
            arguments = self.parse_expressions(tokens[1:], [])        
        op = OpExpression(OpExpression.PREDICATION, arguments)
        if not isGround and not predicate.term_type in [Term.RESOURCE, Term.VARIABLE]:
            self.syntax_exception("Found illegal term '%s' where resource expected" % predicate.value, tokens[0])
        op.predicate = predicate
        op.is_ground = isGround
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
            return self.parse_predication(tokens[1:], predicate=tokens[0])
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
                self.syntax_exception("Found parenthesized expression where term expected: '%s'" % tokens_to_string(tokens), beginToken) 
        elif tokenType == Token.RESERVED_WORD:
            if value in CONNECTIVE_OPERATORS:
                if self.infix_parse:
                    self.syntax_exception("Found operator '%s' where term expected" % tokens_to_string(tokens), beginToken)
                if value in BOOLEAN_OPERATORS and not is_boolean:
                    self.syntax_exception("Found boolean expression where value expression expected '%s'" % tokens_to_string(tokens), beginToken)
                if value in VALUE_OPERATORS and is_boolean:
                    self.syntax_exception("Found value expression where boolean expression expected '%s'" % tokens_to_string(tokens), beginToken)
                ## NOT SURE ABOUT THIS (ESPECIALLY FOR INFIX):
                isBoolean = (value in BOOLEAN_OPERATORS and not value in COMPARISON_OPERATORS)
                arguments = self.parse_expressions(tokens[1:], [], is_boolean=isBoolean)
                return OpExpression(value, arguments)
            elif value in [OpExpression.TRUE, OpExpression.FALSE]:
                if not is_boolean:
                    self.syntax_exception("Found boolean expression where term expected '%s'" % tokens_to_string(tokens), beginToken)
                elif len(tokens) > 1:
                    self.syntax_exception("Found bizarre expression '%s'" % tokens_to_string(tokens), beginToken)
                return OpExpression(value, [])
            elif value in ['LIST', 'SET', 'BAG']:
                return self.parse_enumeration(value, tokens[1:])
            else:
                raise Exception("Unimplemented reserved word '%s'" % value)
        if self.infix_parse:
            return self.parse_infix_expression(tokens, [], connective=None, needs_another_argument=True, is_boolean='UNKNOWN')
        elif is_boolean is True:
                ## THIS ERROR MESSAGE WORKED ONCE.  NEED TO FIND OUT IF IT'S TOO SPECIFIC:
                self.syntax_exception("Illegal expression %s where prefix expression expected"  % tokens_to_string(tokens), tokens)
        else:
            ## THIS IS BOGUS SO FAR.  
            return self.parse_function_expression(tokens)    
        
    def parse_unary_connective(self, tokens, expressions, connective=None):
        """
        Infix mode needs special handling for the NOT operator (because its a prefix operator).
        TODO: FIGURE OUT IF 'OPTIONAL' NEEDS SIMILAR HANDLING HERE
        TODO: GENERALIZE TO ALSO HANDLE 'IN'???
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
            return self.parse_bracket(tokens, expressions, is_boolean=is_boolean)
        elif tokenType == Token.RESERVED_WORD:
            if Token.reserved_type(beginToken, [OpExpression.TRUE, OpExpression.FALSE]):
                expressions.append(OpExpression(beginToken.value, []))
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
                    return self.parse_bracket(tokens[1:], arguments, connective=connective, predicate=beginToken, is_boolean=is_boolean)
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
            
    def parse_where_clause(self, string):
        """
        Parse string into a single expression, or a list of expressions.
        If the latter, convert the list into an AND expression.
        """
        offset = self.source_query.find(string)
        tokens = tokenize(string, [], offset=offset)
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
        
    def parse_limit_clause(self, string):
        offset = self.source_query.find(string)
        tokens = tokenize(string, [], offset=offset)
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
        ## is it prefix or infix:
        if query[0] == '(':
            if not query[len(query) - 1] == ')':
                raise QuerySyntaxException("Missing right parenthesis at the end of query:\n" + query)
            beginPos = 1
            endPos = len(query) - 1
        else:
            beginPos = 0
            endPos = len(query)
        ## THIS IS ALL A CROCK, SINCE THESE WORDS COULD OCCUR WITHIN STRINGS.
        ## NEED TO TOKENIZE THE ENTIRE STRING:
        fromPos = query.find('from')
        wherePos = query.find('where')
        limitPos = query.find('limit')
        if wherePos < 0:
            self.syntax_exception("Missing WHERE clause in query '%s'" % self.source_query)
        if fromPos and fromPos > wherePos:
            self.syntax_exception("FROM clause must precede WHERE clause" % self.source_query)
        selectEndPos = fromPos if fromPos > 0 else wherePos
        selectString = self.source_query[beginPos + len('select'):selectEndPos]
        fromString = self.source_query[fromPos + len('from'):wherePos] if fromPos > 0 else ''
        endWherePos = limitPos if limitPos >0 else endPos
        whereString = self.source_query[wherePos + len('where'):endWherePos]
        limitString = self.source_query[limitPos + len('limit'):endPos] if limitPos > 0 else ''        
        qb = QueryBlock()
        self.parse_tree = qb
        qb.select_terms = self.parse_select_clause(selectString)
        qb.from_list = self.parse_from_clause(fromString)
        qb.where_clause = self.parse_where_clause(whereString)
        qb.limit = self.parse_limit_clause(limitString) if limitString else -1


###########################################################################################################
## Normalization
###########################################################################################################

class Normalizer:
    def __init__(self, parse_tree, language):
        self.parse_tree = parse_tree
        self.language = language
        self.variable_counter = -1
        self.recompute_backlinks()
        
    def normalize(self):
        if self.language == CommonLogicTranslator.PROLOG:
            self.normalize_for_prolog()
        elif self.language == CommonLogicTranslator.SPARQL:
            self.normalize_for_sparql()
    
    def help_walk(self, node, parent, processor, types, bottom_up):
        if type(node) == str: return
        if not bottom_up and (not types or isinstance(node, types)):
            processor(node, parent)
        if isinstance(node, OpExpression):
            if node.predicate and (not types or isinstance(node.predicate, types)):
                self.help_walk(node.predicate, node, processor, types, bottom_up)
            for arg in node.arguments:
                self.help_walk(arg, node, processor, types, bottom_up)
        if bottom_up and (not types or isinstance(node, types)):
            processor(node, parent)
    
    def walk(self, processor, types=None, start_node=None, bottom_up=False):
        """
        Walk the parse tree an apply 'processor' to each node whose type is in 'types'.
        """
        if start_node and not start_node == self.parse_tree:
            self.help_walk(start_node, start_node.parent, processor, types, bottom_up)
        for arg in self.parse_tree.select_terms:
            self.help_walk(arg, self.parse_tree.select_terms, processor, types, bottom_up)
        self.help_walk(self.parse_tree.where_clause, self.parse_tree, processor, types, bottom_up)
    
    def recompute_backlinks(self, where_clause_only=False):
        def add_backlinks(node, parent):
            node.parent = parent        
        self.walk(add_backlinks, start_node=(self.parse_tree.where_clause if where_clause_only else None))
    
    def bump_variable_counter(self, node, parent):
        if not node.term_type == Term.VARIABLE: return
        value = node.value
        if len(value) < 2: return
        if not value.startswith('v') or not value[1:].isdigit(): return
        intVal = int(value[1:])
        self.parse_tree.variable_counter = max(self.parse_tree.variable_counter, intVal)
    
    def get_fresh_variable(self):
        if self.variable_counter == -1:
            self.variable_counter = 0
            self.walk(self.bump_variable_counter, types=Term)
        self.variable_counter += 1
        return "?v%i" % self.variable_counter
    
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

#    @staticmethod    
#    def is_atomic(node):
#        if isinstance(node, Term): return True
#        elif isinstance(node, OpExpression): 
#            return node.operator in [OpExpression.TRUE, OpExpression.FALSE]
        
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
        def flatten(node, parent):
            if not node.operator == OpExpression.AND: return
            conjuncts = []
            for arg in node.arguments:
                if isinstance(arg, OpExpression) and arg.operator == OpExpression.AND:
                    conjuncts.extend(arg.arguments)
                    flattenedSomething = True
                else:
                    conjuncts.append(arg)
            node.arguments = conjuncts
                                    
        self.walk(flatten, types=OpExpression, bottom_up=True)
        if flattenedSomething:
            self.recompute_backlinks(where_clause_only=True)
 
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
        
    def translate_in_enumerate_into_disjunction_of_equalities(self):
        didIt = False
        def doit(node, parent):
            if node.operator == OpExpression.IN:
                ## assumes that the second arg is an enumeration:
                vbl = node.arguments[0]
                enumeration = node.arguments[1]
                equalities = [OpExpression(OpExpression.EQUALITY, [vbl, item]) for item in enumeration.arguments]
                orOp = OpExpression(OpExpression.OR, equalities)
                self.substitute_node(node, orOp)
                didIt = True
        self.walk(doit, types=OpExpression)
        if didIt:
            self.recompute_backlinks()

    ###########################################################################################################
    ## Language-specific Normalization Scripts
    ###########################################################################################################
    
    def normalize_for_prolog(self):
        self.flatten_select_terms()
        self.flatten_value_computations()
        ## TEMPORARY TO SEE WHAT IT LOOKS LIKE:
        self.translate_in_enumerate_into_disjunction_of_equalities()
        ## END TEMPORARY

    def normalize_for_sparql(self):
        """
        Reorganize the parse tree to be compatible with SPARQL's bizarre syntax.
        """
        self.translate_in_enumerate_into_disjunction_of_equalities()
    

###########################################################################################################
##
###########################################################################################################

class StringsBuffer:
    NEWLINE = '\n'
    def __init__(self, include_newlines=False, groundify_all=False, infix_with_prefix_triples=False):
        self.buffer = []
        self.include_newlines = include_newlines
        self.running_indent = 0
        self.infix_with_prefix_triples = infix_with_prefix_triples  # infix CommonLogic hack
        self.groundify_all = groundify_all  # Prolog hack
    
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
        self.append(operator + "!!! ")
        return self            
    
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
                self.common_logify(term.predicate or term.operator.lower()).append(' ')
                self.common_logify(term.arguments)
                self.append(')') 
        return self

    def prologify(self, term, brackets=None, delimiter=' ', suppress_parentheses=False, groundify_all=True):
        if isinstance(term, Term):
            if term.term_type == Term.RESOURCE:
                self.append('!').append(str(term))
            else:
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
            self.groundify_all = groundify_all
            self.append('(select ')    
            if term.distinct: self.complain(term, 'DISTINCT')     
            self.indent(8).prologify(term.select_terms, brackets=('(', ')'), delimiter=' ').newline()   
            self.prologify(term.where_clause, suppress_parentheses=True)
            if term.limit >= 0: self.complain(term, 'LIMIT')
            self.append(')')
        elif isinstance(term, OpExpression):
            if term.operator == OpExpression.ENUMERATION:
                self.append('(')
                self.prologify(term.predicate.lower()).append(' ')
                self.prologify(term.arguments, delimiter=' ')
                self.append(')')  
            elif term.operator == OpExpression.AND:
                if suppress_parentheses:
                    self.prologify(term.arguments, delimiter='\n')
                else:
                    self.append('(and ').prologify(term.arguments, delimiter='\n').append(')')
            elif term.operator in COMPARISON_OPERATORS and not term.operator == '=':
                op = term.operator.lower()
                self.append('(lispp (').append(op).append(' ').prologify(term.arguments[0]).append(' ')
                self.prologify(term.arguments[1]).append(')')
            elif term.is_ground:
                self.append('(q ')
                self.prologify(term.arguments, delimiter=' ')
                self.append(')')
            elif term.predicate and self.groundify_all:
                self.append('(q')
                self.append(' ').prologify(term.arguments[0]).append(' ').prologify(term.predicate)
                self.append(' ').prologify(term.arguments[1])
                if len(term.arguments) > 3:
                    self.append(' ').prologify(term.arguments[2])
                self.append(')')
            elif term.operator == OpExpression.COMPUTE:
                self.append('(lisp ').prologify(term.arguments[0]).append(' (')
                self.prologify(term.arguments[1:], delimiter=' ').append('))')
            else:
                self.append('(')
                self.prologify(term.predicate or term.operator.lower()).append(' ')
                self.prologify(term.arguments, delimiter=' ')
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
                self.infix_common_logify(term.predicate)
                self.infix_common_logify(term.arguments, brackets=('(', ')'))
            else:
                self.append('(')
                self.infix_common_logify(term.predicate or term.operator.lower()).append(' ')
                self.infix_common_logify(term.arguments)
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
                sampleArg = term.arguments[0]
                while sampleArg.operator in [OpExpression.AND, OpExpression.OR]:
                    sampleArg = sampleArg.arguments[0]
                if sampleArg.predicate:
                    brackets = ('{ ', ' }') if not suppress_curlies else None
                    delimiter = ' .\n' if term.operator == OpExpression.AND else '\nunion '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter)     
                else:
                    if not suppress_filter: self.append('filter ')
                    brackets = ('(', ')')
                    delimiter = ' && ' if term.operator == OpExpression.AND else ' || '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter, suppress_filter=True)
            elif term.operator == OpExpression.OPTIONAL:
                if not suppress_curlies: self.append('{')              
                self.append('optional ').sparqlify(term.arguments[0])
                if not suppress_curlies: self.append('}')      
            elif term.operator == OpExpression.NOT:
                self.complain(term, "NOT")
            elif term.is_ground:
                brackets = ('{ ', ' }') if not suppress_curlies else None
                self.sparqlify(term.arguments, brackets=brackets, delimiter=' ')
            elif term.predicate:
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
        return self


###########################################################################################################
## 
###########################################################################################################

def translate_common_logic_query(query, preferred_language='PROLOG'):
    """
    Translate a Common Logic query into either SPARQL or PROLOG syntax.  If 'preferred_language,
    choose that one (first).  Return three
    values, the query, either 'SPARQL' or 'PROLOG', and an error message if the
    translation fails.  It may fail either because the syntax is illegal, or because
    the combination of expressions in the query is not implementable in either SPARQL
    or PROLOG
    """
    def help_translate(language):
        trans = CommonLogicTranslator(query)
        trans.parse()
        Normalizer(trans.parse_tree, language).normalize()
        if language == CommonLogicTranslator.PROLOG:
            translation = str(StringsBuffer(groundify_all=True).prologify(trans.parse_tree))
        elif language == CommonLogicTranslator.SPARQL:
            translation = str(StringsBuffer().sparqlify(trans.parse_tree))
        return translation
    
    try:
        translation = help_translate(preferred_language)
        successfulLanguage = preferred_language
    except QuerySyntaxException, e1:
        try:
            otherLanguage = 'SPARQL' if preferred_language == 'PROLOG' else 'PROLOG'
            translation = help_translate(otherLanguage)
            successfulLanguage = otherLanguage
        except QuerySyntaxException:
            return None, None, e1
    return translation, successfulLanguage, None
    

###########################################################################################################
## Testing
###########################################################################################################


def translate(cl_select_query, target_dialect=CommonLogicTranslator.PROLOG, infix=False):
    trans = CommonLogicTranslator(cl_select_query)
    trans.parse()
    print "\nCOMMON LOGIC \n" + str(StringsBuffer(include_newlines=True).common_logify(trans.parse_tree))    
    print "\nINFIX COMMON LOGIC \n" + str(StringsBuffer(include_newlines=True, infix_with_prefix_triples=trans.infix_with_prefix_triples).infix_common_logify(trans.parse_tree))
    Normalizer(trans.parse_tree, CommonLogicTranslator.SPARQL).normalize()        
    print "\nSPARQL \n" + str(StringsBuffer(include_newlines=True).sparqlify(trans.parse_tree))
    trans = CommonLogicTranslator(cl_select_query)
    trans.parse()    
    Normalizer(trans.parse_tree, CommonLogicTranslator.PROLOG).normalize()
    print "\nPROLOG \n" + str(StringsBuffer(include_newlines=True, groundify_all=True).prologify(trans.parse_tree))    


query1 = """(select (?s ?o) where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>))"""
query1i = """select ?s ?o where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)"""
query2 = """(select (?s ?o) where (and (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)))"""
query2i = """select ?s ?o where ex:name(?s ?o) and rdf:type(?s <http://www.franz.com/example#Person>)"""
query3 = """(select (?s ?o) where (and (ex:name ?s ?o) (= ?o "Fred")))"""
query3i = """select ?s ?o where (ex:name ?s ?o) and (?o = "Fred")"""
query4 = """(select (?s ?o) where (and (or (ex:name ?s ?o) (ex:title ?s ?o)) (= ?o "Fred")))"""
query4i = """select ?s ?o where ((ex:name ?s ?o) or (ex:title ?s ?o))and (?o = "Fred")"""
query5 = """select (?s) where (and (ex:name ?s ?o) (or (= ?o "Fred") (= ?o "Joe")))"""
query5i = """select ?s where (ex:name ?s ?o) and ((?o = "Fred") or (?o = "Joe"))"""
query6 = """select (?s ?o) where (and (ex:name ?s ?o) (not (rdf:type ?s <http://www.franz.com/example#Person>)))"""
query6i = """select ?s ?o where (ex:name ?s ?o) and not (rdf:type ?s <http://www.franz.com/example#Person>)"""
query7 = """select (?s ?o) where (and (triple ?s ex:name ?o) (triple ?s rdf:type <http://www.franz.com/example#Person>))"""
query7i = """select ?s ?o where triple(?s ex:name ?o) and triple(?s rdf:type <http://www.franz.com/example#Person>)"""
query8 = """select (?s ?o) where (and (quad ?s ex:name ?o ?c) (= ?c ex:cxt))"""
query8i = """select ?s ?o where quad(?s ex:name ?o ?c) and (?c = ex:cxt)"""
query9 = """select (?s ?age) where (and (ex:age ?s ?age) (>= ?age 21))"""
query9i = """select ?s ?age where ex:age(?s ?age) and (?age >= 21)"""
query10 = """select (?name ?age) where (and (triple ?s rdf:type ex:Person) (optional (triple ?s ex:name ?name)) 
                                             (optional (and (ex:age ?s ?age) (> ?age 21))))"""
query10i = """select ?name ?age where triple(?s rdf:type ex:Person) and optional (triple ?s ex:name ?name) 
                                       and optional (and ex:age(?s ?age) (?age > 21))"""
query11 = """select (?s ?o) where (and (((ex:name ?s ?o))) (((triple ?s rdfs:label ?o))))"""
query11i = """select ?s ?o where ((ex:name(?s ?o))) and ((triple(?s rdfs:label ?o)))"""  ## triple part screws up
query12 = """select (?name) where (and (rdf:type ?c ex:Company) (ex:gross ?c ?gross) (ex:expenses ?c ?expenses)
                                                (> (- ?gross ?expenses) 50000) (ex:name ?c ?name))"""
query12i = """select ?name where rdf:type(?c ex:Company) and ex:gross(?c ?gross) and ex:expenses(?c ?expenses)
                                                and ((?gross - ?expenses) > 5000) and ex:name(?c ?name)"""
query13 = """(select (?s ?p ?o) where (triple ?s ?p ?o) (in ?s (list <http://foo> <http://bar>)))"""
query13i = """select ?s ?p ?o where triple(?s ?p ?o) and ?s in [<http://foo> <http://bar>]"""   
query14 = """(select distinct (?s ?p ?o) where (triple ?s ?p ?o) limit 5)"""                                            
query14i = """select distinct (?s ?p ?o) where triple(?s ?p ?o) limit 5"""                                            

    
if __name__ == '__main__':
    switch = 14
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
    elif switch == 6: translate(query6) # NEGATION.  SPARQL BREAKS
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
    elif switch == 13: translate(query13)  # IN ENUMERATION
    elif switch == 13.1: translate(query13i)        
    elif switch == 14: translate(query14)  # DISTINCT AND LIMIT
    elif switch == 14.1: translate(query14i)        
    else:
        print "There is no test number %s" % switch

