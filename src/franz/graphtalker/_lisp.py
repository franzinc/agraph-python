"""Internal module for building Lisp expressions and parsing Lisp results.

This module handles the bidirectional translation between Python values
and Lisp string representations used by the eval server.

The eval server evaluates expressions via (eval (read-from-string expression))
and returns results formatted via (format nil "~S" value).
"""

import re
from typing import Any

# ---------------------------------------------------------------------------
# Expression building
# ---------------------------------------------------------------------------


def escape_lisp_string(s: str) -> str:
    """Escape a Python string for embedding in a Lisp double-quoted string.

    Lisp's reader only needs backslash and double-quote escaped.
    All other characters (newlines, Unicode, etc.) are valid inside Lisp strings.
    Order matters: backslash first to avoid double-escaping.
    """
    s = s.replace("\\", "\\\\")
    s = s.replace('"', '\\"')
    return s


def lisp_string(s: str) -> str:
    """Format a Python string as a Lisp string literal."""
    return f'"{escape_lisp_string(s)}"'


def lisp_call(func_name: str, *args: str) -> str:
    """Build a Lisp function call from pre-formatted argument strings."""
    if args:
        return f"({func_name} {' '.join(args)})"
    return f"({func_name})"


def lisp_keyword_arg(name: str, value: str) -> str:
    """Format a Lisp keyword argument like :name value."""
    return f":{name} {value}"


def lisp_int(n: int) -> str:
    """Format an integer as a Lisp literal."""
    return str(n)


def lisp_bool(b: bool) -> str:
    """Format a boolean as Lisp T/NIL."""
    return "t" if b else "nil"


def lisp_setf(variable: str, value: str) -> str:
    """Build a (setf *variable* value) expression."""
    return f"(setf {variable} {value})"


def lisp_jso(**kwargs: str) -> str:
    """Build a st-json jso expression from key-value pairs.

    Values must be pre-formatted Lisp strings.
    Example: lisp_jso(query=lisp_string("SELECT...")) -> '(jso "query" "SELECT...")'
    """
    parts = []
    for key, val in kwargs.items():
        parts.append(f'"{key}" {val}')
    return f"(jso {' '.join(parts)})"


def lisp_execute_tool(tool_name: str, **params: str) -> str:
    """Build an (execute-tool "name" (jso ...)) expression.

    Param values must be pre-formatted Lisp strings.
    """
    jso = lisp_jso(**params)
    return f"(execute-tool {lisp_string(tool_name)} {jso})"


# ---------------------------------------------------------------------------
# Result parsing
# ---------------------------------------------------------------------------


def parse_lisp_result(result_str: str) -> Any:
    """Parse a Lisp ~S formatted result string into a Python value.

    Handles:
    - "T"         -> True
    - "NIL"       -> None
    - "3"         -> 3 (int)
    - "3.14"      -> 3.14 (float)
    - '"hello"'   -> "hello" (unquoted string)
    - "(:KEY1 val1 :KEY2 val2)" -> {"key1": val1, "key2": val2}
    - Anything else -> original string (passthrough)
    """
    if result_str is None:
        return None

    s = result_str.strip()
    if not s:
        return None

    # Boolean/nil (case-insensitive: Lisp may use T/NIL or t/nil)
    s_upper = s.upper()
    if s_upper == "T":
        return True
    if s_upper == "NIL":
        return None

    # Integer
    if re.fullmatch(r"-?\d+", s):
        return int(s)

    # Float (Lisp can return 3.14, 3.14d0, 3.14e2, etc.)
    if re.fullmatch(r"-?\d+\.\d+([dDeEfFsSlL][+-]?\d+)?", s):
        normalized = re.sub(r"[dDfFsSlL]", "e", s)
        return float(normalized)

    # Quoted string: "some text here"
    if s.startswith('"') and s.endswith('"') and len(s) >= 2:
        return _unescape_lisp_string(s[1:-1])

    # Plist: (:KEY1 val1 :KEY2 val2 ...)
    if s.startswith("(") and ":" in s:
        try:
            return _parse_plist(s)
        except Exception:
            return s

    # Fallback: return as-is
    return s


def _unescape_lisp_string(s: str) -> str:
    """Unescape a Lisp string (reverse of escape_lisp_string)."""
    result = []
    i = 0
    while i < len(s):
        if s[i] == "\\" and i + 1 < len(s):
            next_char = s[i + 1]
            if next_char == '"':
                result.append('"')
            elif next_char == "\\":
                result.append("\\")
            elif next_char == "n":
                result.append("\n")
            elif next_char == "t":
                result.append("\t")
            elif next_char == "r":
                result.append("\r")
            else:
                result.append(next_char)
            i += 2
        else:
            result.append(s[i])
            i += 1
    return "".join(result)


def _parse_plist(s: str) -> dict:
    """Parse a Lisp plist string into a Python dict.

    Input:  "(:CUMULATIVE-INPUT-TOKENS 1234 :PERCENTAGE 45.2)"
    Output: {"cumulative_input_tokens": 1234, "percentage": 45.2}

    Converts :KEYWORD-NAMES to python_snake_case keys.
    """
    inner = s.strip()[1:-1].strip()
    tokens = _tokenize_lisp(inner)

    result = {}
    i = 0
    while i < len(tokens) - 1:
        token = tokens[i]
        if token.startswith(":"):
            key = token[1:].lower().replace("-", "_")
            value = parse_lisp_result(tokens[i + 1])
            result[key] = value
            i += 2
        else:
            i += 1

    return result


def _tokenize_lisp(s: str) -> list:
    """Tokenize a Lisp expression, respecting quoted strings and nested parens."""
    tokens = []
    i = 0
    while i < len(s):
        c = s[i]
        # Skip whitespace
        if c in " \t\n\r":
            i += 1
            continue
        # Quoted string
        if c == '"':
            j = i + 1
            while j < len(s):
                if s[j] == "\\":
                    j += 2
                elif s[j] == '"':
                    break
                else:
                    j += 1
            tokens.append(s[i : j + 1])
            i = j + 1
        # Nested paren expression
        elif c == "(":
            depth = 1
            j = i + 1
            while j < len(s) and depth > 0:
                if s[j] == "(":
                    depth += 1
                elif s[j] == ")":
                    depth -= 1
                elif s[j] == '"':
                    j += 1
                    while j < len(s) and s[j] != '"':
                        if s[j] == "\\":
                            j += 1
                        j += 1
                j += 1
            tokens.append(s[i:j])
            i = j
        # Regular token
        else:
            j = i
            while j < len(s) and s[j] not in " \t\n\r()":
                j += 1
            tokens.append(s[i:j])
            i = j
    return tokens
