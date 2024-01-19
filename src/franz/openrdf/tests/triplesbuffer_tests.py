import pytest

from franz.openrdf.model import Literal
from franz.openrdf.tests.conftest import conn, ex
from franz.openrdf.util.triples_buffer import TriplesBuffer, buffered_triples


def test_empty_triples_buffer(conn):
    buf = TriplesBuffer(conn)
    assert len(buf) == 0


def test_triples_buffer_basic(conn, ex):
    limit = 1000
    buf = TriplesBuffer(conn, limit=limit)
    triple = [ex("Alice"), ex("age"), Literal(29)]

    for _ in range(limit):
        buf.append(triple)

    assert len(buf) == limit and conn.size() == 0

    buf.flush()

    assert len(buf) == 0 and conn.size() == limit


def test_triples_buffer_above_limit(conn, ex):
    limit = 10
    buf = TriplesBuffer(conn, limit=limit)
    triple = [ex("Alice"), ex("age"), Literal(29)]

    for _ in range(limit * 3):
        buf.append(triple)

    assert len(buf) == 10 and conn.size() == 20

    buf.append(triple)

    assert len(buf) == 1 and conn.size() == 30

    buf.flush()

    assert len(buf) == 0 and conn.size() == 31


def test_buffered_addTriple(conn, ex):
    limit = 1000
    count = limit * 3
    triple = [ex("Alice"), ex("age"), Literal(29)]

    with buffered_triples(conn, limit=limit) as buf:
        for _ in range(count):
            buf.append(triple)

    assert len(buf) == 0 and conn.size() == count
