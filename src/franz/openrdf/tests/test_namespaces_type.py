"""
Unit tests for the scope parameter on namespace methods of RepositoryConnection.
These tests do not require a live AllegroGraph server.
"""

from unittest.mock import MagicMock, patch

import pytest

from franz.openrdf.repository.repositoryconnection import RepositoryConnection


@pytest.fixture
def conn():
    """A RepositoryConnection with a mocked mini-repository."""
    connection = MagicMock(spec=RepositoryConnection)
    mini_repo = MagicMock()
    connection._get_mini_repository.return_value = mini_repo
    # Bind the real methods to this mock instance so validation logic runs
    connection.getNamespaces = RepositoryConnection.getNamespaces.__get__(connection)
    connection.getNamespace = RepositoryConnection.getNamespace.__get__(connection)
    connection.setNamespace = RepositoryConnection.setNamespace.__get__(connection)
    connection.removeNamespace = RepositoryConnection.removeNamespace.__get__(
        connection
    )
    connection.clearNamespaces = RepositoryConnection.clearNamespaces.__get__(
        connection
    )
    return connection


@pytest.fixture
def mini(conn):
    """The mock mini-repository underlying conn."""
    return conn._get_mini_repository()


# ---------------------------------------------------------------------------
# getNamespaces
# ---------------------------------------------------------------------------


class TestGetNamespaces:
    def test_default_scope_is_user(self, conn, mini):
        mini.listNamespaces.return_value = [
            {"prefix": "ex", "namespace": "http://example.org/"}
        ]
        result = conn.getNamespaces()
        mini.listNamespaces.assert_called_once_with("user")
        assert result == {"ex": "http://example.org/"}

    @pytest.mark.parametrize("scope", ["user", "repository", "default", "any"])
    def test_valid_scopes(self, conn, mini, scope):
        mini.listNamespaces.return_value = []
        conn.getNamespaces(scope)
        mini.listNamespaces.assert_called_once_with(scope)

    def test_invalid_scope_raises(self, conn):
        with pytest.raises(
            ValueError, match='"user", "repository", "default" or "any"'
        ):
            conn.getNamespaces("bogus")

    def test_returns_dict(self, conn, mini):
        mini.listNamespaces.return_value = [
            {
                "prefix": "rdf",
                "namespace": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            },
            {"prefix": "owl", "namespace": "http://www.w3.org/2002/07/owl#"},
        ]
        result = conn.getNamespaces("any")
        assert result == {
            "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "owl": "http://www.w3.org/2002/07/owl#",
        }


# ---------------------------------------------------------------------------
# getNamespace
# ---------------------------------------------------------------------------


class TestGetNamespace:
    def test_default_scope_is_user(self, conn, mini):
        mini.getNamespace.return_value = "http://example.org/"
        conn.getNamespace("ex")
        mini.getNamespace.assert_called_once_with("ex", "user")

    @pytest.mark.parametrize("scope", ["user", "repository", "default"])
    def test_valid_scopes(self, conn, mini, scope):
        mini.getNamespace.return_value = "http://example.org/"
        conn.getNamespace("ex", scope)
        mini.getNamespace.assert_called_once_with("ex", scope)

    def test_invalid_scope_raises(self, conn):
        with pytest.raises(ValueError, match='"user", "repository" or "default"'):
            conn.getNamespace("ex", "any")

    def test_any_scope_not_accepted(self, conn):
        with pytest.raises(ValueError):
            conn.getNamespace("ex", "any")

    def test_returns_namespace_uri(self, conn, mini):
        mini.getNamespace.return_value = "http://example.org/"
        result = conn.getNamespace("ex", "user")
        assert result == "http://example.org/"


# ---------------------------------------------------------------------------
# setNamespace
# ---------------------------------------------------------------------------


class TestSetNamespace:
    def test_default_scope_is_user(self, conn, mini):
        conn.setNamespace("ex", "http://example.org/")
        mini.addNamespace.assert_called_once_with("ex", "http://example.org/", "user")

    @pytest.mark.parametrize("scope", ["user", "repository", "default"])
    def test_valid_scopes(self, conn, mini, scope):
        conn.setNamespace("ex", "http://example.org/", scope)
        mini.addNamespace.assert_called_once_with("ex", "http://example.org/", scope)

    def test_invalid_scope_raises(self, conn):
        with pytest.raises(ValueError, match='"user", "repository" or "default"'):
            conn.setNamespace("ex", "http://example.org/", "bogus")

    def test_any_scope_not_accepted(self, conn):
        with pytest.raises(ValueError):
            conn.setNamespace("ex", "http://example.org/", "any")


# ---------------------------------------------------------------------------
# removeNamespace
# ---------------------------------------------------------------------------


class TestRemoveNamespace:
    def test_default_scope_is_user(self, conn, mini):
        conn.removeNamespace("ex")
        mini.deleteNamespace.assert_called_once_with("ex", "user")

    @pytest.mark.parametrize("scope", ["user", "repository", "default"])
    def test_valid_scopes(self, conn, mini, scope):
        conn.removeNamespace("ex", scope)
        mini.deleteNamespace.assert_called_once_with("ex", scope)

    def test_invalid_scope_raises(self, conn):
        with pytest.raises(ValueError, match='"user", "repository" or "default"'):
            conn.removeNamespace("ex", "bogus")

    def test_any_scope_not_accepted(self, conn):
        with pytest.raises(ValueError):
            conn.removeNamespace("ex", "any")


# ---------------------------------------------------------------------------
# clearNamespaces
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------


class TestClearNamespaces:
    def test_default_scope_is_user(self, conn, mini):
        conn.clearNamespaces()
        mini.clearNamespaces.assert_called_once_with("user", True)

    def test_default_reset_is_true(self, conn, mini):
        conn.clearNamespaces("repository")
        mini.clearNamespaces.assert_called_once_with("repository", True)

    @pytest.mark.parametrize("scope", ["user", "repository", "default"])
    def test_valid_scopes(self, conn, mini, scope):
        conn.clearNamespaces(scope)
        mini.clearNamespaces.assert_called_once_with(scope, True)

    @pytest.mark.parametrize("reset", [True, False])
    def test_reset_flag_forwarded(self, conn, mini, reset):
        conn.clearNamespaces("user", reset)
        mini.clearNamespaces.assert_called_once_with("user", reset)

    def test_invalid_scope_raises(self, conn):
        with pytest.raises(ValueError, match='"user", "repository" or "default"'):
            conn.clearNamespaces("bogus")

    def test_any_scope_not_accepted(self, conn):
        with pytest.raises(ValueError):
            conn.clearNamespaces("any")
