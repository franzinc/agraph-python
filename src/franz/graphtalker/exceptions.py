"""Exception hierarchy for the GraphTalker client."""


class GraphTalkerError(Exception):
    """Base exception for all GraphTalker errors."""

    pass


class ConnectionError(GraphTalkerError):
    """Failed to connect to the eval server."""

    pass


class AuthenticationError(GraphTalkerError):
    """Authentication failed (401 from eval server)."""

    pass


class EvalError(GraphTalkerError):
    """Lisp evaluation raised an error.

    Attributes:
        lisp_error: The error message from Lisp.
        stdout: Any output produced before the error.
    """

    def __init__(self, message: str, stdout: str = ""):
        super().__init__(message)
        self.lisp_error = message
        self.stdout = stdout


class QueryAbortedError(EvalError):
    """Query was aborted by the user via abort_query().

    Subclass of EvalError so existing except EvalError handlers
    still catch it, but callers can distinguish aborts if needed.
    """

    pass


class TimeoutError(GraphTalkerError):
    """Request to eval server timed out."""

    pass


class ServerError(GraphTalkerError):
    """Eval server returned an unexpected HTTP error.

    Attributes:
        status_code: The HTTP status code.
        body: The response body.
    """

    def __init__(self, status_code: int, body: str):
        super().__init__(f"HTTP {status_code}: {body}")
        self.status_code = status_code
        self.body = body
