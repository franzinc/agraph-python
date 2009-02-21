#!/usr/bin/env python

###########################################################################################################
## RDF Constants
###########################################################################################################

class XSD:
    NS = "http://www.w3.org/2001/XMLSchema#"
    STRING = NS + "string"
    BOOLEAN = NS + "boolean"
    INTEGER = NS + "integer"
    INT = NS + "int"
    LONG = NS + "long"    
    FLOAT = NS + "float"        
    DOUBLE = NS + "double"
    DATE = NS + "date"
    DATETIME = NS + "datetime"
    TIME = NS + "time"
    NUMBER = NS + 'number' ## NOT SURE ABOUT THIS
    URL = NS + 'anyURI'
    
###########################################################################################################
## Exceptions
###########################################################################################################

class QuerySyntaxException(Exception):
    "Illegal Common Logic syntax"
    
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
    RESERVED_WORD_SET = set(['and', 'or', 'not', 'true', 'false', '=', '<', '>', '<=', '>=', '!=', '+', '-', 'triple', 'quad'])
    
    def __init__(self, token_type, value, offset=None):
        if token_type == Token.RESERVED_WORD:
            value = OperatorExpression.parse_operator(value)
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
    if word.lower() in Token.RESERVED_WORD_SET:
        tokens.append(Token(Token.RESERVED_WORD, word))
    elif word[0].isdigit():
        tokens.append(Token(Token.NUMBER, word))
    elif word.find(':') >=0:
        tokens.append(Token(Token.QNAME, word))
    else:
        raise QuerySyntaxException("Unrecognized term '%s'" % word)
    return string[endPos:]

def tokenize(string, tokens, offset=0):
    """
    Parse 'string' into tokens.  Push tokens into 'tokens' during recursion,
    and return 'tokens'.
    """
    string = string.strip()
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
FROM = 'from'
WHERE = 'where'

class QueryBlock:
    def __init__(self, query_type=SELECT):
        query_type = query_type
        self.select_terms = None
        self.from_list = None
        self.where_clause = None
        
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

COMPARISON_OPERATORS = set(['=', '<', '>', '<=', '>=', '!='])
BOOLEAN_OPERATORS = set(['AND', 'OR', 'NOT', 'IN']).union(COMPARISON_OPERATORS)
VALUE_OPERATORS = set(['PLUS', 'MINUS'])
CONNECTIVE_OPERATORS = BOOLEAN_OPERATORS.union(VALUE_OPERATORS)
OPERATOR_EXPRESSIONS = CONNECTIVE_OPERATORS.union(set(['ENUMERATION', 'TRUE', 'FALSE', 'TRIPLE', 'QUAD'])) # omits 'PREDICATION'

class OperatorExpression:
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'
    IN = 'IN'
    ENUMERATION = 'ENUMERATION'
    PREDICATION = 'PREDICATION'
    TRUE = 'TRUE'
    FALSE = 'FALSE'
    EQUALITY = '='
    TRIPLE = 'TRIPLE'
    QUAD = 'QUAD'
    ## MAY NOT NEED TO BE EXPLICIT ABOUT THESE:
    PLUS = '+'
    MINUS = '-'
    
    def __init__(self, operator, arguments):
        self.operator = operator
        self.arguments = arguments
        self.predicate = None
        self.is_ground = False
    
    @staticmethod
    def parse_operator(value):
        return value.upper() if value.upper() in OPERATOR_EXPRESSIONS else value
        
    def __str__(self):
        return str(StringsBuffer().common_logify(self))

INFIX_WITH_PREFIX_TRIPLES = True

class CommonLogicTranslator:
    """
    """
    SPARQL = 'SPARQL'
    PROLOG = 'PROLOG'
    
    def __init__(self, infix=False):
        self.source_query = None
        self.parse_tree = None
        self.infix_parse = infix
        self.infix_with_prefix_triples = infix and INFIX_WITH_PREFIX_TRIPLES
        ## TEMPORARY PROLOG HACK
        self.all_ground = False        
        
    def syntax_exception(self, message, token=None):
        if isinstance(token, list):
            token = token[0] if token else None
        supplement = ''
        if token:
            supplement = "Error occurred at offset %i in the string \n    %s" % (token.offset, self.source_query)
            SHIM = 3
            pointer = '    ' + ' ' * (token.offset + 1 + SHIM) + '^' 
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
            if token.value.isnum():
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
        tokens = tokenize(select_string, [])
        if not self.infix_parse:
            if len(tokens) >= 2 and tokens[0].value == '(' and tokens[len(tokens) - 1].value == ')':
                tokens = tokens[1:-1]
            else:
                self.syntax_exception("Missing parentheses around select clause arguments")
        terms = []
        for t in tokens:
            ## TODO: ALLOW ARBITRARY EXPRESSIONS HERE: 
            if not t.token_type in [Token.URI, Token.STRING, Token.NUMBER, Token.VARIABLE, Token.QNAME]:
                self.syntax_exception("Illegal operator '%s' found in select clause" % t.value, t)
            terms.append(self.token_to_term(t))
        return terms
        
    def parse_from_clause(self, from_string):
        """
        NOT YET IMPLEMENTED
        """
        return None        
    
    def parse_enumeration(self, tokens, is_boolean):
        if is_boolean is True:
            self.syntax_exception("Enumeration found where boolean expected:  [%s]" % tokens_to_string(tokens, comma_delimited=True), tokens)
        arguments = self.parse_expressions(tokens, [])
        return OperatorExpression(OperatorExpression.ENUMERATION, arguments)
    
    def parse_predication(self, tokens, predicate=None):
        """
        'tokens' represent a predicate applied to arguments
        """
        if Token.reserved_type(predicate, [OperatorExpression.TRIPLE, OperatorExpression.QUAD]):
            predicate = predicate.value.lower()
            isGround = True
        elif self.all_ground: ## HACK FOR TESTING PROLOG
            isGround = True
            predicate = OperatorExpression.TRIPLE.lower()
        else:
            isGround = False
        if predicate:
            arguments = self.parse_expressions(tokens, [])
        else:
            predicate = self.token_to_term(tokens[0])            
            arguments = self.parse_expressions(tokens[1:], [])        
        op = OperatorExpression(OperatorExpression.PREDICATION, arguments)
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
            return self.parse_enumeration(tokens, is_boolean)
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
        elif  Token.reserved_type(tokens[0], [OperatorExpression.TRIPLE, OperatorExpression.QUAD]):
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
                return OperatorExpression(value, arguments)
            elif value in [OperatorExpression.TRUE, OperatorExpression.FALSE]:
                if not is_boolean:
                    self.syntax_exception("Found boolean expression where term expected '%s'" % tokens_to_string(tokens), beginToken)
                elif len(tokens) > 1:
                    self.syntax_exception("Found bizarre expression '%s'" % tokens_to_string(tokens), beginToken)
                return OperatorExpression(value, [])
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
        
    def parse_not(self, tokens, expressions, connective=None):
        """
        Infix mode needs special handling for the NOT operator (because its a prefix operator).
        TODO: FIGURE OUT IF 'OPTIONAL' NEEDS SIMILAR HANDLING HERE
        TODO: GENERALIZE TO ALSO HANDLE 'IN'???
        """
        if len(tokens) < 2:
            self.syntax_exception("Insufficient arguments to negation operator", tokens)
        beginToken = tokens[1]
        beginVal = beginToken.value
        if beginVal == '(':
            return self.parse_bracket(tokens[1:], expressions, connective=connective, 
                                      unary_operator=OperatorExpression.NOT, is_boolean=True)
        else:
            self.syntax_exception("NOT operator expects a parenthesized argument", tokens)        
    
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
                    exp = OperatorExpression(unary_operator, [exp])
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
            if Token.reserved_type(beginToken, [OperatorExpression.TRUE, OperatorExpression.FALSE]):
                expressions.append(OperatorExpression(beginToken.value, []))
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
                return OperatorExpression(connective, arguments)
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
                    return self.parse_bracket(tokens[1:], arguments, connective=connective, predicate=beginToken, is_boolean=is_boolean)
                elif Token.reserved_type(beginToken, [OperatorExpression.TRIPLE, OperatorExpression.QUAD]):
                    return self.parse_bracket(tokens[1:], arguments, connective=connective, predicate=beginToken, is_boolean=is_boolean)
            if tokenType in ATOMIC_TERM_TOKEN_TYPES:
                if is_boolean is True:
                    self.syntax_exception("Value term found where boolean expression expected '%s'" % beginToken.value, beginToken)
                arguments.append(self.token_to_term(beginToken))
                return self.parse_infix_expression(tokens[1:], arguments, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
            if tokenType in set([Token.VARIABLE]):
                arguments.append(self.token_to_term(beginToken))
                return self.parse_infix_expression(tokens[1:], arguments, connective=connective, needs_another_argument=False, is_boolean=is_boolean)
            if tokenType == Token.BRACKET:
                return self.parse_bracket(tokens, arguments, connective=connective, is_boolean=is_boolean)
            elif Token.reserved_type(beginToken, OperatorExpression.NOT) and is_boolean:
                return self.parse_not(tokens, arguments, connective=connective)
            elif Token.reserved_type(beginToken, [OperatorExpression.TRUE, OperatorExpression.FALSE]) and is_boolean:
                    arguments.append(OperatorExpression(beginToken.value, []))
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
            arguments = [OperatorExpression(connective, arguments)]
        isBoolean = nextConnective in BOOLEAN_OPERATORS and not nextConnective in COMPARISON_OPERATORS
        return self.parse_infix_expression(tokens[1:], arguments, connective=nextConnective, is_boolean=isBoolean)
   
    def parse_where_clause(self, string):
        """
        Parse string into a single expression, or a list of expressions.
        If the latter, convert the list into an AND expression.
        """
        offset = self.source_query.find(string)
        tokens = tokenize(string, [], offset=offset)
        if self.infix_parse:
            expressions = [self.parse_infix_expression(tokens, [], connective=None, is_boolean=True)]
        else:
            expressions = self.parse_expressions(tokens, [], is_boolean=True)
        if not expressions:
            self.syntax_exception("Query has empty where clause")            
        elif len(expressions) == 1:
            return expressions[0]
        else:
            return OperatorExpression(OperatorExpression.AND, expressions)

    def parse(self):
        """
        Parse 'source_query' into a parse tree.
        FOR NOW, ASSUME ITS A 'select' QUERY
        """
        self.clean_source()
        query = self.source_query.lower()
        fromPos = query.find('from')
        wherePos = query.find('where')
        if wherePos < 0:
            self.syntax_exception("Missing WHERE clause in query '%s'" % self.source_query)
        if fromPos and fromPos > wherePos:
            self.syntax_exception("FROM clause must precede WHERE clause" % self.source_query)
        selectEndPos = fromPos if fromPos > 0 else wherePos
        selectString = self.source_query[len('select'):selectEndPos]
        fromString = self.source_query[fromPos + len('from'):wherePos] if fromPos > 0 else ''
        whereString = self.source_query[wherePos + len('where'):]
        qb = QueryBlock()
        qb.infix_with_prefix_triples = self.infix_with_prefix_triples # hack to communicate with StringsBuffer
        self.parse_tree = qb
        qb.select_terms = self.parse_select_clause(selectString)
        qb.from_list = self.parse_from_clause(fromString)
        qb.where_clause = self.parse_where_clause(whereString)

    ###########################################################################################################
    ## SPARQL Translation
    ###########################################################################################################
    
    def normalize_to_sparql(self):
        """
        Reorganize the parse tree to be compatible with SPARQL's bizarre syntax.
        """
    
    def generate_sparql(self):
        self.normalize_to_sparql()
        
###########################################################################################################
##
###########################################################################################################

class StringsBuffer:
    NEWLINE = '\n'
    def __init__(self, include_newlines=False):
        self.buffer = []
        self.include_newlines = include_newlines
        self.running_indent = 0
        self.infix_with_prefix_triples = False
    
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
            self.append('select ')         
            self.append('(').common_logify(term.select_terms, delimiter=' ').append(')\n')
            self.append('where ')
            self.indent(6).common_logify(term.where_clause)
        elif isinstance(term, OperatorExpression):
            if term.operator == OperatorExpression.ENUMERATION:
                self.common_logify(term.arguments, brackets=('[', ']'), delimiter=', ')
            elif term.operator in [OperatorExpression.AND, OperatorExpression.OR]:
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

    def prologify(self, term, brackets=None, delimiter=' ', suppress_parentheses=False):
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
            self.append('(select ')         
            self.indent(8).prologify(term.select_terms, brackets=('(', ')'), delimiter=' ').newline()   
            self.prologify(term.where_clause, suppress_parentheses=True)
            self.append(')')
        elif isinstance(term, OperatorExpression):
            if term.operator == OperatorExpression.ENUMERATION:
                self.prologify(term.arguments, brackets=('[', ']'), delimiter=', ')
            elif term.operator == OperatorExpression.AND:
                if suppress_parentheses:
                    self.prologify(term.arguments, delimiter='\n')
                else:
                    self.append('(and ').prologify(term.arguments, delimiter='\n').append(')')
            elif term.is_ground:
                self.append('(q ')
                self.prologify(term.arguments, delimiter=' ')
                self.append(')')
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
            self.infix_with_prefix_triples = term.infix_with_prefix_triples
            self.append('select ')         
            self.infix_common_logify(term.select_terms, delimiter=' ').newline()
            self.append('where ')
            self.indent(6).infix_common_logify(term.where_clause, suppress_parentheses=True)
        elif isinstance(term, OperatorExpression):
            if term.operator == OperatorExpression.ENUMERATION:
                self.infix_common_logify(term.arguments, brackets=('[', ']'), delimiter=', ')
            elif term.operator in [OperatorExpression.AND, OperatorExpression.OR]:                
                brackets = ('(', ')') if not suppress_parentheses else None
                self.infix_common_logify(term.arguments, delimiter='\n%s ' % term.operator.lower(), brackets=brackets)
            elif term.operator == OperatorExpression.NOT:
                if not suppress_parentheses: self.append('(')              
                self.append('not ').infix_common_logify(term.arguments[0])
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
            self.sparqlify(term.select_terms, delimiter=' ').newline()   
            self.append('where ').append('{ ')
            self.indent(6).sparqlify(term.where_clause, suppress_curlies=True)
            self.append(' }')
        elif isinstance(term, OperatorExpression):
            if term.operator in [OperatorExpression.AND, OperatorExpression.OR]:
                ## are we joining triples or filters? look at first non-connective in descendants
                ## to find out:
                sampleArg = term.arguments[0]
                while sampleArg.operator in [OperatorExpression.AND, OperatorExpression.OR]:
                    sampleArg = sampleArg.arguments[0]
                if sampleArg.predicate:
                    brackets = ('{ ', ' }') if not suppress_curlies else None
                    delimiter = ' .\n' if term.operator == OperatorExpression.AND else '\nunion '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter)     
                else:
                    if not suppress_filter: self.append('filter ')
                    brackets = ('(', ')')
                    delimiter = ' && ' if term.operator == OperatorExpression.AND else ' || '
                    self.sparqlify(term.arguments, brackets=brackets, delimiter=delimiter, suppress_filter=True)
            elif term.operator == OperatorExpression.NOT:
                self.sparqlify("NOT!!!")
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



def translate(cl_select_query, target_dialect=CommonLogicTranslator.PROLOG, all_quads=False, infix=False):
    trans = CommonLogicTranslator(infix=infix)
    trans.source_query = cl_select_query
    trans.all_ground = all_quads
    trans.parse()
    print "\nCOMMON LOGIC \n" + str(StringsBuffer(include_newlines=True).common_logify(trans.parse_tree))    
    print "\nINFIX COMMON LOGIC \n" + str(StringsBuffer(include_newlines=True).infix_common_logify(trans.parse_tree))        
    print "\nSPARQL \n" + str(StringsBuffer(include_newlines=True).sparqlify(trans.parse_tree))
    print "\nPROLOG \n" + str(StringsBuffer(include_newlines=True).prologify(trans.parse_tree))    



###########################################################################################################
## Testing
###########################################################################################################


query1 = """select (?s ?o) where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)"""
query1i = """select ?s ?o where (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>)"""
query2 = """select (?s ?o) where (and (ex:name ?s ?o) (rdf:type ?s <http://www.franz.com/example#Person>))"""
query2i = """select ?s ?o where ex:name(?s ?o) and rdf:type(?s <http://www.franz.com/example#Person>)"""
query3 = """select (?s ?o) where (and (ex:name ?s ?o) (= ?o "Fred"))"""
query3i = """select ?s ?o where (ex:name ?s ?o) and (?o = "Fred")"""
query4 = """select (?s ?o) where (and (or (ex:name ?s ?o) (ex:title ?s ?o)) (= ?o "Fred"))"""
query4i = """select ?s ?o where ((ex:name ?s ?o) or (ex:title ?s ?o))and (?o = "Fred")"""
query5 = """select (?s) where (and (ex:name ?s ?o) (or (= ?o "Fred") (= ?o "Joe")))"""
query5i = """select ?s where (ex:name ?s ?o) and ((?o = "Fred") or (?o = "Joe"))"""
query6 = """select (?s ?o) where (and (ex:name ?s ?o) (not (rdf:type ?s <http://www.franz.com/example#Person>)))"""
query6i = """select ?s ?o where (ex:name ?s ?o) and not (rdf:type ?s <http://www.franz.com/example#Person>)"""
query7 = """select (?s ?o) where (and (triple ?s ex:name ?o) (triple ?s rdf:type <http://www.franz.com/example#Person>))"""
query7i = """select ?s ?o where triple(?s ex:name ?o) and triple(?s rdf:type <http://www.franz.com/example#Person>)"""

query7 = """select (?s ?o) where (triple ?s ex:name ?o) """
query7i = """select ?s ?o where triple(?s ex:name ?o) """




    
if __name__ == '__main__':
    switch = 7
    print "Running test", switch
    if switch == 1: translate(query1, all_quads=True)  # IMPLICIT AND
    elif switch == 1.1: translate(query1i, infix=True, all_quads=True)
    elif switch == 2: translate(query2, all_quads=True) # EXPLICIT AND
    elif switch == 2.1: translate(query2i, infix=True, all_quads=True)
    elif switch == 3: translate(query3, infix=False, all_quads=True) # EQUALITY
    elif switch == 3.1: translate(query3i, infix=True, all_quads=True)        
    elif switch == 4: translate(query4, infix=False, all_quads=True) # TRIPLE DISJUNCTION
    elif switch == 4.1: translate(query4i, infix=True, all_quads=True)        
    elif switch == 5: translate(query5, infix=False, all_quads=True) # FILTER DISJUNCTION
    elif switch == 5.1: translate(query5i, infix=True, all_quads=True)        
    elif switch == 6: translate(query6, infix=False, all_quads=True) # NEGATION.  SPARQL BREAKS
    elif switch == 6.1: translate(query6i, infix=True, all_quads=True)        
    elif switch == 7: translate(query7, infix=False, all_quads=True) # TRIPLE PREDICATE
    elif switch == 7.1: translate(query7i, infix=True, all_quads=True)        
    else:
        print "There is no test number %s" % switch

