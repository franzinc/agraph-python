# -*- coding: utf-8 -*-

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from datetime import datetime, date, time, timedelta
from decimal import Decimal

import pytest
import os
import sys

import re
from six import BytesIO

from franz.openrdf.connect import ag_connect
from franz.openrdf.exceptions import RequestError
from franz.openrdf.model import Literal, Statement
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.tupleformat import TupleFormat
from franz.openrdf.sail import AllegroGraphServer
from franz.openrdf.tests.tz import MockTimezone
from franz.openrdf.util.contexts import output_to
from franz.openrdf.vocabulary import XMLSchema

from .tests import AG_HOST, AG_PORT, AG_PROXY, CATALOG, STORE, USER, PASSWORD

common_args = dict(
    host=AG_HOST, port=AG_PORT, catalog=CATALOG,
    user=USER, password=PASSWORD, proxy=AG_PROXY
)


def get_statements(conn):
    """
    Gets all statements as a list of lists of Value objects,
    sorted lexicographically.
    """
    return sorted([list(s) for s in conn.getStatements()])


def test_ag_connect_open(repo_name):
    with ag_connect(repo_name, create=False, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_create(non_existing_repo):
    with ag_connect(non_existing_repo, create=True, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_recreate(conn):
    store = conn.repository.database_name
    with conn:
        conn.addTriple('<http://franz.com/s>', '<http://franz.com/p>', '<http://franz.com/o>')
    with ag_connect(store, clear=True, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_open_no_create(non_existing_repo):
    with pytest.raises(Exception):
        ag_connect(non_existing_repo, create=False, **common_args)


def test_ag_connect_create_exists():
    with pytest.raises(Exception):
        ag_connect(STORE, create=True, fail_if_exists=True, **common_args)


def test_ag_connect_fail_if_exists_but_not_create(repo_name):
    with ag_connect(repo_name, create=False, fail_if_exists=True, **common_args) as conn:
        assert conn.size() == 0


def test_ag_connect_session(repo_name):
    with ag_connect(repo_name, create=False, session=True, **common_args) as conn:
        assert conn.is_session_active


def test_server_all_data_in_host(clean_env):
    server = AllegroGraphServer('https://somehost:4321/and/then/some')
    assert server.url == 'https://somehost:4321/and/then/some'


def test_server_all_defaults(clean_env):
    server = AllegroGraphServer('somehost')
    assert server.url == 'http://somehost:10035'


def test_server_protocol_as_arg(clean_env):
    server = AllegroGraphServer('somehost', protocol='https')
    assert server.url == 'https://somehost:10036'


def test_server_override_protocol(clean_env):
    server = AllegroGraphServer('http://somehost', protocol='https')
    assert server.url == 'https://somehost:10036'


def test_server_port_as_arg(clean_env):
    server = AllegroGraphServer('somehost', port=4321)
    assert server.url == 'http://somehost:4321'


def test_server_override_port(clean_env):
    server = AllegroGraphServer('somehost:1234', port=4321)
    assert server.url == 'http://somehost:4321'


def test_server_https_if_cainfo(clean_env):
    server = AllegroGraphServer('somehost', cainfo='/path/to/ca/bundle')
    assert server.url == 'https://somehost:10036'


def test_server_url_in_env(clean_env):
    clean_env['AGRAPH_HOST'] = 'somehost'
    clean_env['AGRAPH_PORT'] = 12345
    clean_env['AGRAPH_USER'] = 'luser'
    clean_env['AGRAPH_PASSWORD'] = '1234'
    server = AllegroGraphServer()
    assert server.url == 'http://somehost:12345'
    assert server._client.user == 'luser'
    assert server._client.password == '1234'


@pytest.mark.parametrize("filename, expected_format, expected_compression", [
    ("somefile.rdf", "RDF/XML", None),
    ("somefile.rdfs", "RDF/XML", None),
    ("somefile.owl", "RDF/XML", None),
    ("somefile.xml", "RDF/XML", None),
    ("somefile.rdf.gz", "RDF/XML", "gzip"),
    ("somefile.rdfs.gz", "RDF/XML", "gzip"),
    ("somefile.owl.gz", "RDF/XML", "gzip"),
    ("somefile.xml.gz", "RDF/XML", "gzip"),
    ("somefile.nt", "N-Triples", None),
    ("somefile.ntriples", "N-Triples", None),
    ("somefile.nt.gz", "N-Triples", "gzip"),
    ("somefile.ntriples.gz", "N-Triples", "gzip"),
    ("somefile.nq", "N-Quads", None),
    ("somefile.nquads", "N-Quads", None),
    ("somefile.nq.gz", "N-Quads", "gzip"),
    ("somefile.nquads.gz", "N-Quads", "gzip"),
    ("somefile.nqx", "Extended N-Quads (with attributes)", None),
    ("somefile.nqx.gz", "Extended N-Quads (with attributes)", "gzip"),
    ("somefile.ttl", "Turtle", None),
    ("somefile.turtle", "Turtle", None),
    ("somefile.ttl.gz", "Turtle", "gzip"),
    ("somefile.turtle.gz", "Turtle", "gzip"),
    ("somefile.trig", "TriG", None),
    ("somefile.trig.gz", "TriG", "gzip"),
    ("somefile.trix", "TriX", None),
    ("somefile.trix.gz", "TriX", "gzip"),
    ("sOmEfIlE.tRiX.gZ", "TriX", "gzip"),
    ("somefile.kaboom", None, None),
])
def test_format_for_ext(filename, expected_format, expected_compression):
    actual_format, actual_compression = RDFFormat.format_for_file_name(filename)
    if actual_format is None:
        assert expected_format is None
    else:
        assert expected_format == actual_format.name
    assert expected_compression == actual_compression


@pytest.mark.parametrize("value, expected_text, expected_type", [
    (u'दुप', u'दुप', None), (b'test', u'test', None),
    (True, 'true', XMLSchema.BOOLEAN), (False, 'false', XMLSchema.BOOLEAN),
    (42, '42', XMLSchema.INTEGER), (42.000, '42.0', XMLSchema.DOUBLE),
    (Decimal('42.000'), '42.000', XMLSchema.DECIMAL),
    (datetime(1984, 8, 26, 10, 0, 5), '1984-08-26T10:00:05Z', XMLSchema.DATETIME),
    # TODO: Should we really be converting times to UTC?
    (datetime(1984, 8, 26, 12, 0, 5, tzinfo=MockTimezone('CEST', 2, 1)),
     '1984-08-26T10:00:05Z', XMLSchema.DATETIME),
    (datetime(1984, 8, 27, 1, 0, 5, tzinfo=MockTimezone('CEST', 2, 1)),
     '1984-08-26T23:00:05Z', XMLSchema.DATETIME),
    (date(1984, 8, 26), '1984-08-26', XMLSchema.DATE),
    (time(10, 0, 5), '10:00:05Z', XMLSchema.TIME),
    (time(12, 0, 5, tzinfo=MockTimezone('CEST', 2, 1)), '10:00:05Z', XMLSchema.TIME),
    (time(1, 0, 5, tzinfo=MockTimezone('CEST', 2, 1)), '23:00:05Z', XMLSchema.TIME),
    ([1, 2, 3], "[1, 2, 3]", None)])
def test_literals_from_python_values(value, expected_text, expected_type):
    literal = Literal(value)
    assert literal.label == expected_text
    # Well-known types are normalized, so it is safe to use the ``is`` operator here.
    assert literal.datatype is expected_type


@pytest.mark.skipif(sys.version_info >= (3,),
                    reason="Long type exists only on Python 2.")
def test_long_literal():
    # This behavior of using LONG as the datatype might be convenient in demos,
    # (you can do ``Literal(42L)``), but has a few drawbacks:
    #    - is incorrect: will map arbitrarily huge values to LONG, which is
    #      supposed to be 64 bit, while SMALLER values will become INTEGERS
    #      (which have no size limit).
    #    - In fact longs start at sys.maxint + 1 == 2 ** 63, i.e. as soon as
    #      it is no longer valid to hold them in xsd:longs.
    #    - Python 3 has no 'long' type.
    literal = Literal(84104105115032109097107101115032110111032115101110115101046)
    assert literal.datatype is XMLSchema.LONG


def test_true_literal():
    assert Literal(True).booleanValue()


def test_false_literal():
    assert not Literal(False).booleanValue()


def test_add_data_ascii(conn):
    base_uri = u'http://franz.com/'
    expected = conn.createURI(namespace=base_uri, localname='x')
    conn.addData(u'<x> <x> <x> .', base_uri=base_uri)
    actual = conn.getStatements(None, None, None, None).asList()
    assert len(actual) == 1
    assert actual[0].getSubject() == expected
    assert actual[0].getPredicate() == expected
    assert actual[0].getObject() == expected


def test_add_data_unicode(conn):
    """ See bug24405. """
    base_uri = u'http://franz.com/'
    expected = conn.createURI(namespace=base_uri, localname=u'दुप')
    conn.addData(u'<दुप> <दुप> <दुप> .', base_uri=base_uri)
    actual = conn.getStatements(None, None, None, None).asList()
    assert len(actual) == 1
    assert actual[0].getSubject() == expected
    assert actual[0].getPredicate() == expected
    assert actual[0].getObject() == expected


def test_get_statements_raw_value(conn):
    conn.addData('<ex://s1> <ex://p1> true .')
    conn.addData('<ex://s2> <ex://p2> false .')
    actual = conn.getStatements(None, None, False, None).asList()
    assert len(actual) == 1
    assert actual[0].getSubject() == conn.createURI("ex://s2")


# See spr43630
def test_unicode_literal_filter(conn, s, p):
    o = conn.createLiteral(u'<दुप>')
    conn.add(s, p, o)
    conn.getStatements(None, None, o)


def test_remove_statement(conn, s, p, o, x, g):
    conn.add(s, p, o, [g])
    conn.add(x, p, o, [g])
    conn.add(s, x, o, [g])
    conn.add(s, p, x, [g])
    conn.add(s, p, o, [x])
    stmt = Statement(s, p, o, g)
    conn.removeStatement(stmt, [g])
    remaining = conn.getStatements(None, None, None).asList()
    assert len(remaining) == 4
    for r in remaining:
        assert r.getSubject() != s or r.getPredicate() != p \
               or r.getObject() != o or r.getContext() != g


def test_no_suppression(conn, s, p, o, o2):
    assert conn.getDuplicateSuppressionPolicy() is None
    conn.add(s, p, o)
    conn.add(s, p, o)
    conn.add(s, p, o2)
    conn.commit()
    assert len(conn.getStatements(None, None, None)) == 3


def test_spo_suppression(conn, s, p, o, o2, g1, g2):
    conn.setDuplicateSuppressionPolicy("spo")
    assert conn.getDuplicateSuppressionPolicy() == "spo"
    conn.add(s, p, o, [g1])
    conn.add(s, p, o, [g1])
    conn.add(s, p, o2, [g1])
    conn.add(s, p, o, [g2])
    conn.commit()
    assert len(conn.getStatements(None, None, None)) == 2


def test_spog_suppression(conn, s, p, o, o2, g1, g2):
    conn.setDuplicateSuppressionPolicy("spog")
    assert conn.getDuplicateSuppressionPolicy() == "spog"
    conn.add(s, p, o, [g1])
    conn.add(s, p, o, [g1])
    conn.add(s, p, o2, [g1])
    conn.add(s, p, o, [g2])
    conn.commit()
    assert len(conn.getStatements(None, None, None)) == 3


def test_disable_suppression(conn, s, p, o, o2):
    conn.setDuplicateSuppressionPolicy("spo")
    conn.add(s, p, o)
    conn.add(s, p, o)
    conn.add(s, p, o2)
    conn.commit()
    conn.disableDuplicateSuppression()
    assert conn.getDuplicateSuppressionPolicy() is None
    conn.add(s, p, o)
    conn.add(s, p, o2)
    assert len(conn.getStatements(None, None, None)) == 4


def test_suppression_none(conn, s, p, o, o2):
    conn.setDuplicateSuppressionPolicy("spo")
    conn.add(s, p, o)
    conn.add(s, p, o)
    conn.add(s, p, o2)
    conn.commit()
    conn.setDuplicateSuppressionPolicy(None)
    assert conn.getDuplicateSuppressionPolicy() is None
    conn.add(s, p, o)
    conn.add(s, p, o2)
    assert len(conn.getStatements(None, None, None)) == 4


def test_switch_suppression(conn, s, p, o, o2, g1, g2):
    conn.setDuplicateSuppressionPolicy("spog")
    assert conn.getDuplicateSuppressionPolicy() == "spog"
    conn.add(s, p, o, [g1])
    conn.add(s, p, o, [g1])
    conn.add(s, p, o2, [g1])
    conn.add(s, p, o, [g2])
    conn.commit()
    conn.setDuplicateSuppressionPolicy("spo")
    assert conn.getDuplicateSuppressionPolicy() == "spo"
    conn.add(s, p, o)
    assert len(conn.getStatements(None, None, None)) == 3


def test_switch_suppression_before_commit(session, s, p, o, o2, g1, g2):
    session.setDuplicateSuppressionPolicy("spog")
    session.add(s, p, o, [g1])
    session.add(s, p, o, [g1])
    session.add(s, p, o2, [g1])
    session.add(s, p, o, [g2])
    session.setDuplicateSuppressionPolicy("spo")
    session.commit()
    assert len(session.getStatements(None, None, None)) == 2


def test_export_ntriples(conn):
    conn.addData("""<ex://s1> <ex://p1> <ex://o1> .
                    <ex://s2> <ex://p2> <ex://o2> .""")
    out = BytesIO()
    conn.getStatements(output=out, output_format=RDFFormat.NTRIPLES)
    result = sorted(out.getvalue().decode('utf-8').splitlines())
    assert result == [
        "<ex://s1> <ex://p1> <ex://o1> .",
        "<ex://s2> <ex://p2> <ex://o2> ."
    ]


def test_export_by_id(conn):
    conn.addData("""<ex://s1> <ex://p1> <ex://o1> .
                    <ex://s2> <ex://p2> <ex://o2> .""")
    ids = [stmt.getTripleID()
           for stmt in conn.getStatements(subject=conn.createURI('ex://s2'), tripleIDs=True)]

    out = BytesIO()
    conn.getStatementsById(ids, output=out, output_format=RDFFormat.NTRIPLES)
    result = sorted(out.getvalue().decode('utf-8').splitlines())
    assert result == [
        "<ex://s2> <ex://p2> <ex://o2> ."
    ]


def test_export_query_result(conn):
    conn.addData("""<ex://s1> <ex://p1> <ex://o1> .
                    <ex://s2> <ex://p2> <ex://o2> .""")
    query_string = "select ?s { ?s <ex://p1> ?o }"
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL, query_string)
    out = BytesIO()
    query.evaluate(output=out, output_format=TupleFormat.CSV)
    result = re.sub(r'\s', '', out.getvalue().decode('utf-8'))
    assert result == 's"ex://s1"'


def test_export_construct(conn):
    query = conn.prepareGraphQuery(QueryLanguage.SPARQL, """
        CONSTRUCT {
          <ex://s1> <ex://p1> <ex://o1> .
          <ex://s2> <ex://p2> <ex://o2> .
        } WHERE {}""")
    out = BytesIO()
    query.evaluate(output=out, output_format=RDFFormat.NTRIPLES)
    result = sorted(out.getvalue().decode('utf-8').splitlines())
    assert result == [
        "<ex://s1> <ex://p1> <ex://o1> .",
        "<ex://s2> <ex://p2> <ex://o2> ."
    ]


def test_datetime_value_tz():
    lit = Literal('1984-08-26T10:00:05+02:00', XMLSchema.DATETIME)
    dt = lit.datetimeValue()
    assert dt.utcoffset() == timedelta(0, 7200)


def test_time_value_tz():
    lit = Literal('1984-08-26T10:00:05+02:00', XMLSchema.DATETIME)
    t = lit.datetimeValue()
    assert t.utcoffset() == timedelta(0, 7200)


def test_datetime_tz_roundtrip(conn, s, p):
    dt = conn.createLiteral('1984-08-26T10:00:05+02:00', XMLSchema.DATETIME)
    conn.addTriple(s, p, dt)
    actual = conn.getStatements().asList()[0].getObject()
    assert actual == dt


def test_time_tz_roundtrip(conn, s, p):
    t = conn.createLiteral('10:00:05+02:00', XMLSchema.TIME)
    conn.addTriple(s, p, t)
    actual = conn.getStatements().asList()[0].getObject()
    assert actual == t


def test_invalid_query(conn):
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL,
                                   '"; DROP TABLE students;')
    with open(os.devnull, 'w') as out:
        with pytest.raises(RequestError):
            query.evaluate(output=out)


def test_default_rdf_format(conn):
    out = BytesIO()
    query = conn.prepareGraphQuery(QueryLanguage.SPARQL,"""
        CONSTRUCT { <ex://s> <ex://p> <ex://o> } WHERE {}""")
    query.evaluate(output=out)
    assert out.getvalue()


def test_default_tuple_format(conn):
    out = BytesIO()
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL,"""
        SELECT ?x { BIND(42 as ?x) }""")
    query.evaluate(output=out)
    assert out.getvalue()


def test_output_to_true(capfd):
    with output_to(True) as f:
        f.write(b'test')
    out, err = capfd.readouterr()
    assert out == 'test'


def test_output_to_stderr(capfd):
    with output_to(2) as f:
        f.write(b'test')
    out, err = capfd.readouterr()
    assert err == 'test'


def test_export_to_true(capfd, conn, s, p, o):
    conn.add(s, p, o)
    conn.getStatements(output=True)
    out, err = capfd.readouterr()
    assert out.strip() == "<ex://s> <ex://p> <ex://o> ."


def test_triple_len(s, p, o):
    assert len(Statement(s, p, o)) == 3


def test_quad_len(s, p, o, g):
    assert len(Statement(s, p, o, g)) == 4


def test_stmt_str(ex):
    # This used to raise an error, see bug25079
    assert 'ex' in str(Statement(ex.s, ex.p, ex.o))


def test_statement_from_strings_subject(ex):
    stmt = Statement('<ex://s>', '<ex://p>', '<ex://o>')
    assert stmt.getSubject() == ex.s


def test_statement_from_strings_predicate(ex):
    stmt = Statement('<ex://s>', '<ex://p>', '<ex://o>')
    assert stmt.getPredicate() == ex.p


def test_statement_from_strings_object(ex):
    stmt = Statement('<ex://s>', '<ex://p>', '<ex://o>')
    assert stmt.getObject() == ex.o


def test_triple_ne_quad(ex):
    s1 = Statement(ex.s, ex.p, ex.o)
    s2 = Statement(ex.s, ex.p, ex.o, ex.g)
    assert s1 != s2


def test_add_triples_statements(conn, ex):
    s = Statement(ex.s, ex.p, ex.o)
    conn.addTriples([s])
    assert conn.getStatements().asList() == [s]


def test_conn_namespace(conn):
    ex = conn.namespace('http://franz.com/example/')
    assert ex.foo == conn.createURI('http://franz.com/example/foo')


def test_add_with_single_context(conn, s, p, o, g):
    conn.addTriples([(s, p, o)], context=g)
    assert get_statements(conn) == [[s, p, o, g]]


def test_add_with_two_contexts(conn, ex):
    conn.addTriples([(ex.s, ex.p, ex.o)], context=[ex.g1, ex.g2])
    assert get_statements(conn) == [[ex.s, ex.p, ex.o, ex.g1],
                                    [ex.s, ex.p, ex.o, ex.g2]]


def test_add_quad_overrides_contexts(conn, ex):
    conn.addTriples([(ex.s, ex.p, ex.o, ex.g)], context=[ex.g1, ex.g2])
    assert get_statements(conn) == [[ex.s, ex.p, ex.o, ex.g]]
