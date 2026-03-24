"""Unit tests for Lisp expression building and result parsing.

These tests are pure Python -- no server required.
"""

import pytest

from franz.graphtalker._lisp import (
    escape_lisp_string,
    lisp_bool,
    lisp_call,
    lisp_execute_tool,
    lisp_int,
    lisp_jso,
    lisp_keyword_arg,
    lisp_setf,
    lisp_string,
    parse_lisp_result,
)

# ---------------------------------------------------------------------------
# String escaping
# ---------------------------------------------------------------------------


class TestEscapeLispString:
    def test_simple_string(self):
        assert escape_lisp_string("hello") == "hello"

    def test_empty_string(self):
        assert escape_lisp_string("") == ""

    def test_double_quotes(self):
        assert escape_lisp_string('say "hello"') == 'say \\"hello\\"'

    def test_backslash(self):
        assert escape_lisp_string("path\\to\\file") == "path\\\\to\\\\file"

    def test_quotes_and_backslash(self):
        assert escape_lisp_string('a "b\\c" d') == 'a \\"b\\\\c\\" d'

    def test_newlines_preserved(self):
        # Newlines don't need escaping in Lisp strings
        assert escape_lisp_string("line1\nline2") == "line1\nline2"

    def test_tabs_preserved(self):
        assert escape_lisp_string("col1\tcol2") == "col1\tcol2"

    def test_sparql_with_quotes(self):
        sparql = 'SELECT ?s WHERE { ?s rdfs:label "Hello World" }'
        escaped = escape_lisp_string(sparql)
        assert escaped == 'SELECT ?s WHERE { ?s rdfs:label \\"Hello World\\" }'

    def test_sparql_multiline(self):
        sparql = """SELECT ?s ?p ?o
WHERE {
    ?s ?p ?o .
    FILTER(?o = "test")
}"""
        escaped = escape_lisp_string(sparql)
        assert '\\"test\\"' in escaped
        assert "\n" in escaped  # newlines preserved


# ---------------------------------------------------------------------------
# Expression builders
# ---------------------------------------------------------------------------


class TestLispString:
    def test_simple(self):
        assert lisp_string("hello") == '"hello"'

    def test_with_quotes(self):
        assert lisp_string('say "hi"') == '"say \\"hi\\""'

    def test_empty(self):
        assert lisp_string("") == '""'


class TestLispCall:
    def test_no_args(self):
        assert lisp_call("test-agraph-connection") == "(test-agraph-connection)"

    def test_one_arg(self):
        expr = lisp_call("set-repository-from-string", lisp_string("demos:test"))
        assert expr == '(set-repository-from-string "demos:test")'

    def test_multiple_args(self):
        expr = lisp_call("foo", '"a"', '"b"', "42")
        assert expr == '(foo "a" "b" 42)'


class TestLispKeywordArg:
    def test_basic(self):
        assert lisp_keyword_arg("continue", "t") == ":continue t"

    def test_nil(self):
        assert lisp_keyword_arg("verbose", "nil") == ":verbose nil"


class TestLispInt:
    def test_positive(self):
        assert lisp_int(42) == "42"

    def test_negative(self):
        assert lisp_int(-7) == "-7"

    def test_zero(self):
        assert lisp_int(0) == "0"


class TestLispBool:
    def test_true(self):
        assert lisp_bool(True) == "t"

    def test_false(self):
        assert lisp_bool(False) == "nil"


class TestLispSetf:
    def test_string_value(self):
        expr = lisp_setf("*claude-model*", lisp_string("claude-sonnet-4-5-20250929"))
        assert expr == '(setf *claude-model* "claude-sonnet-4-5-20250929")'

    def test_int_value(self):
        expr = lisp_setf("*max-tokens*", lisp_int(32000))
        assert expr == "(setf *max-tokens* 32000)"

    def test_bool_value(self):
        expr = lisp_setf("*enable-prompt-caching*", lisp_bool(True))
        assert expr == "(setf *enable-prompt-caching* t)"


class TestLispJso:
    def test_single_param(self):
        expr = lisp_jso(query=lisp_string("SELECT * WHERE { ?s ?p ?o }"))
        assert expr == '(jso "query" "SELECT * WHERE { ?s ?p ?o }")'

    def test_multiple_params(self):
        expr = lisp_jso(
            title=lisp_string("My Query"),
            description=lisp_string("A test query"),
        )
        assert '"title" "My Query"' in expr
        assert '"description" "A test query"' in expr


class TestLispExecuteTool:
    def test_sparql_query(self):
        expr = lisp_execute_tool(
            "sparql_query", query=lisp_string("SELECT ?s WHERE { ?s ?p ?o }")
        )
        assert expr == (
            '(execute-tool "sparql_query" (jso "query" "SELECT ?s WHERE { ?s ?p ?o }"))'
        )

    def test_store_query(self):
        expr = lisp_execute_tool(
            "store_query",
            title=lisp_string("Test"),
            description=lisp_string("A test"),
            sparqlQuery=lisp_string("SELECT 1"),
        )
        assert '"store_query"' in expr
        assert '"title" "Test"' in expr
        assert '"sparqlQuery" "SELECT 1"' in expr


# ---------------------------------------------------------------------------
# Result parsing
# ---------------------------------------------------------------------------


class TestParseLispResult:
    def test_true(self):
        assert parse_lisp_result("T") is True

    def test_nil(self):
        assert parse_lisp_result("NIL") is None

    def test_none_input(self):
        assert parse_lisp_result(None) is None

    def test_empty_string(self):
        assert parse_lisp_result("") is None

    def test_whitespace(self):
        assert parse_lisp_result("  ") is None

    def test_integer(self):
        assert parse_lisp_result("42") == 42

    def test_negative_integer(self):
        assert parse_lisp_result("-7") == -7

    def test_zero(self):
        assert parse_lisp_result("0") == 0

    def test_float(self):
        assert parse_lisp_result("3.14") == pytest.approx(3.14)

    def test_negative_float(self):
        assert parse_lisp_result("-2.5") == pytest.approx(-2.5)

    def test_lisp_double_float(self):
        # Lisp can return 3.14d0 for double-float
        assert parse_lisp_result("3.14d0") == pytest.approx(3.14)

    def test_scientific_notation(self):
        assert parse_lisp_result("1.5e2") == pytest.approx(150.0)

    def test_quoted_string(self):
        assert parse_lisp_result('"hello world"') == "hello world"

    def test_quoted_string_with_escapes(self):
        assert parse_lisp_result('"say \\"hi\\""') == 'say "hi"'

    def test_quoted_empty_string(self):
        assert parse_lisp_result('""') == ""

    def test_simple_plist(self):
        result = parse_lisp_result("(:KEY-ONE 42 :KEY-TWO 3.14)")
        assert result == {"key_one": 42, "key_two": pytest.approx(3.14)}

    def test_plist_with_nil(self):
        result = parse_lisp_result("(:KEY-ONE 42 :KEY-TWO NIL)")
        assert result == {"key_one": 42, "key_two": None}

    def test_plist_with_string(self):
        result = parse_lisp_result('(:NAME "hello" :COUNT 5)')
        assert result == {"name": "hello", "count": 5}

    def test_context_stats_plist(self):
        plist = (
            "(:CUMULATIVE-INPUT-TOKENS 5000"
            " :CUMULATIVE-OUTPUT-TOKENS 1200"
            " :CURRENT-CONTEXT-TOKENS 4500"
            " :MESSAGE-COUNT 8"
            " :CONTEXT-LIMIT 200000"
            " :PERCENTAGE 2.25"
            " :CACHE-CREATION-TOKENS 300"
            " :CACHE-READ-TOKENS 1500)"
        )
        result = parse_lisp_result(plist)
        assert result["cumulative_input_tokens"] == 5000
        assert result["cumulative_output_tokens"] == 1200
        assert result["current_context_tokens"] == 4500
        assert result["message_count"] == 8
        assert result["context_limit"] == 200000
        assert result["percentage"] == pytest.approx(2.25)
        assert result["cache_creation_tokens"] == 300
        assert result["cache_read_tokens"] == 1500

    def test_unknown_passthrough(self):
        raw = "#<The agraph-claude-tools package>"
        assert parse_lisp_result(raw) == raw

    def test_lisp_object_passthrough(self):
        raw = "#<STANDARD-CLASS FOO @ #x7f...>"
        assert parse_lisp_result(raw) == raw

    def test_t_with_whitespace(self):
        assert parse_lisp_result("  T  ") is True

    def test_nil_with_whitespace(self):
        assert parse_lisp_result("  NIL  ") is None


# ---------------------------------------------------------------------------
# Round-trip tests (build expression, parse expected result)
# ---------------------------------------------------------------------------


class TestRoundTrip:
    def test_sparql_with_embedded_quotes(self):
        """SPARQL containing double quotes survives escaping."""
        sparql = (
            'SELECT ?s WHERE { ?s rdfs:label "John O\'Brien" . FILTER(?s != "test") }'
        )
        expr = lisp_execute_tool("sparql_query", query=lisp_string(sparql))
        # Verify the expression is well-formed (has matching parens)
        assert expr.count("(") == expr.count(")")
        # Verify the inner quotes are escaped
        assert '\\"John O\'Brien\\"' in expr

    def test_multiline_sparql(self):
        """Multiline SPARQL query is properly embedded."""
        sparql = "SELECT ?s ?p ?o\nWHERE {\n  ?s ?p ?o\n}\nLIMIT 10"
        expr = lisp_execute_tool("sparql_query", query=lisp_string(sparql))
        assert expr.count("(") == expr.count(")")
        assert "\n" in expr  # newlines preserved in the expression
