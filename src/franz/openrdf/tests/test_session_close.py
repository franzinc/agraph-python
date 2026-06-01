from unittest import mock

import pytest

from franz.miniclient.repository import Service
from franz.openrdf.repository.repository import Repository
from franz.openrdf.repository.repositoryconnection import RepositoryConnection


class TestServiceClose:
    def test_close_when_session_is_none(self):
        svc = Service("http://localhost:10035")
        svc.close()

    def test_close_closes_session_and_nulls_it(self):
        svc = Service("http://localhost:10035")
        fake_session = mock.Mock()
        svc.session = fake_session

        svc.close()

        fake_session.close.assert_called_once()
        assert svc.session is None

    def test_close_idempotent(self):
        svc = Service("http://localhost:10035")
        fake_session = mock.Mock()
        svc.session = fake_session

        svc.close()
        svc.close()

        fake_session.close.assert_called_once()
        assert svc.session is None


class TestRepositoryShutDown:
    def test_shut_down_calls_mini_repository_close(self):
        fake_mini = mock.Mock(spec=["close"])
        repo = mock.MagicMock(spec=Repository)
        repo.mini_repository = fake_mini

        Repository.shutDown(repo)

        fake_mini.close.assert_called_once()
        assert repo.mini_repository is None

    def test_shut_down_when_mini_repository_is_none(self):
        repo = mock.MagicMock(spec=Repository)
        repo.mini_repository = None

        Repository.shutDown(repo)

        assert repo.mini_repository is None


class TestRepositoryConnectionCloseSessionCleanup:
    def test_close_with_close_repo_calls_shut_down(self):
        fake_mini = mock.Mock()
        fake_mini.session = None
        fake_repo = mock.Mock()
        fake_repo.mini_repository = fake_mini
        fake_repo.shutDown = mock.Mock()

        conn = RepositoryConnection(fake_repo, close_repo=True)

        conn.close()

        fake_repo.shutDown.assert_called_once()

    def test_close_with_close_repo_false_does_not_call_shut_down(self):
        fake_mini = mock.Mock()
        fake_mini.session = None
        fake_repo = mock.Mock()
        fake_repo.mini_repository = fake_mini
        fake_repo.shutDown = mock.Mock()

        conn = RepositoryConnection(fake_repo, close_repo=False)

        conn.close()

        fake_repo.shutDown.assert_not_called()

    def test_close_with_close_repo_false_does_not_close_session(self):
        # Shared connections (close_repo=False, e.g. repo.getConnection()) must
        # not close the session -- other connections sharing the same
        # mini_repository may still be using it.
        svc = Service("http://localhost:10035")
        fake_session = mock.Mock()
        svc.session = fake_session

        fake_repo = mock.MagicMock(spec=Repository)
        fake_repo.mini_repository = svc

        conn = RepositoryConnection(fake_repo, close_repo=False)
        conn.close()

        fake_session.close.assert_not_called()
        assert svc.session is fake_session

    def test_with_statement_calls_close(self):
        fake_mini = mock.Mock()
        fake_mini.session = None
        fake_repo = mock.Mock()
        fake_repo.mini_repository = fake_mini
        fake_repo.shutDown = mock.Mock()

        with RepositoryConnection(fake_repo, close_repo=True) as conn:
            assert not conn.is_closed

        assert conn.is_closed
        fake_repo.shutDown.assert_called_once()


class TestServiceCloseViaShutDownIntegration:
    def test_session_is_closed_through_shut_down_chain(self):
        svc = Service("http://localhost:10035")
        fake_session = mock.Mock()
        svc.session = fake_session

        class FakeRepo:
            pass

        # Simulate the Repository wrapping the Service
        repo = FakeRepo()
        repo.mini_repository = svc

        Repository.shutDown(repo)

        fake_session.close.assert_called_once()
        assert svc.session is None
        assert repo.mini_repository is None
