from pygments.lexer import RegexLexer, bygroups, inherit, using
from pygments.lexers.python import PythonLexer
from pygments.lexers.rdf import SparqlLexer, TurtleLexer
from pygments.token import *

# TODO: remove redundancy + support escapes in strings
# TODO: this deletes some whitespace, add missing groups


class CustomLexer(PythonLexer):
    tokens = {
        "root": [
            (
                r'(addData)\s*(\()\s*(r?""")',
                bygroups(Name, Punctuation, String.Double),
                "turtle-long-double",
            ),
            (
                r"(addData)\s*(\()\s*(r?''')",
                bygroups(Name, Punctuation, String.Single),
                "turtle-long-single",
            ),
            (
                r'(prepare\S*Query)\s*(\()(\s*)(QueryLanguage.SPARQL)\s*(,)(\s*)(r?""")',
                bygroups(
                    Name, Punctuation, Text, Name, Punctuation, Text, String.Double
                ),
                "sparql-long-double",
            ),
            (
                r"(prepare\S*Query)\s*(\()(\s*)(QueryLanguage.SPARQL)\s*(,)(\s*)(r?''')",
                bygroups(
                    Name, Punctuation, Text, Name, Punctuation, Text, String.Single
                ),
                "sparql-long-single",
            ),
            (
                r'(addData)\s*(\()(\s)*(r?")',
                bygroups(Name, Punctuation, Text, String.Double),
                "turtle-short-double",
            ),
            (
                r"(addData)\s*(\()(\s*)(r?')",
                bygroups(Name, Punctuation, Text, String.Single),
                "turtle-short-single",
            ),
            (
                r'(prepare\S*Query)\s*(\()(\s*)(QueryLanguage.SPARQL)\s*(,)(\s*)(r?")',
                bygroups(
                    Name, Punctuation, Text, Name, Punctuation, Text, String.Double
                ),
                "sparql-short-double",
            ),
            (
                r"(prepare\S*Query)\s*(\()(\s*)(QueryLanguage.SPARQL)\s*(,)(\s*)(r?')",
                bygroups(
                    Name, Punctuation, Text, Name, Punctuation, Text, String.Single
                ),
                "sparql-short-single",
            ),
            (
                r'(prepare\S*Query)\s*(\()(\s*)(query)\s*(=)\s*(r?""")',
                bygroups(
                    Name, Punctuation, Text, Name.Attribute, Operator, String.Double
                ),
                "sparql-long-double",
            ),
            (
                r"(prepare\S*Query)\s*(\()(\s*)(query)\s*(=)\s*(r?''')",
                bygroups(
                    Name, Punctuation, Text, Name.Attribute, Operator, String.Single
                ),
                "sparql-long-single",
            ),
            (
                r'(execute\S*?Query)\s*(\()(\s*)(r?""")',
                bygroups(Name, Punctuation, Text, String.Double),
                "sparql-long-double",
            ),
            (
                r"(execute\S*?Query)\s*(\()(\s*)(r?''')",
                bygroups(Name, Punctuation, Text, String.Double),
                "sparql-long-single",
            ),
            (
                r'(execute\S*?Query)\s*(\()(\s*)(r?")',
                bygroups(Name, Punctuation, Text, String.Double),
                "sparql-short-double",
            ),
            (
                r"(execute\S*?Query)\s*(\()(\s*)(r?')",
                bygroups(Name, Punctuation, Text, String.Single),
                "sparql-short-single",
            ),
            inherit,
        ],
        "turtle-long-double": [
            (r'([\s\S]+?)(""")', bygroups(using(TurtleLexer), String.Double), "#pop"),
        ],
        "turtle-long-single": [
            (r"([\s\S]+?)(''')", bygroups(using(TurtleLexer), String.Single), "#pop"),
        ],
        "sparql-long-double": [
            (r'([\s\S]+?)(""")', bygroups(using(SparqlLexer), String.Double), "#pop"),
        ],
        "sparql-long-single": [
            (r"([\s\S]+?)(''')", bygroups(using(SparqlLexer), String.Single), "#pop"),
        ],
        "turtle-short-double": [
            (r'([\s\S]+?)(")', bygroups(using(TurtleLexer), String.Double), "#pop"),
        ],
        "turtle-short-single": [
            (r"([\s\S]+?)(')", bygroups(using(TurtleLexer), String.Single), "#pop"),
        ],
        "sparql-short-double": [
            (r'([\s\S]+?)(")', bygroups(using(SparqlLexer), String.Double), "#pop"),
        ],
        "sparql-short-single": [
            (r"([\s\S]+?)(')", bygroups(using(SparqlLexer), String.Single), "#pop"),
        ],
    }
