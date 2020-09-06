# -*- coding: utf-8 -*-

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
import json
from datetime import datetime, date, time, timedelta
from decimal import Decimal

import pytest
import requests
import io
import os
import sys

import re
from six import BytesIO

from future.utils import iteritems, iterkeys
from nose.tools import assert_raises

from franz.openrdf.connect import ag_connect
from franz.openrdf.exceptions import RequestError
from franz.openrdf.model import Literal, Statement, URI
from franz.openrdf.query.query import QueryLanguage
from franz.openrdf.repository.attributes import AttributeDefinition, TripleAttribute, UserAttribute, \
    attribute_filter_to_expr, And, Or, Not, Empty, Overlap, Subset, Superset, Equal, Lt, Le, Eq, Ge, Gt, \
    attribute_set_to_expr
from franz.openrdf.repository.transactions import TransactionSettings
from franz.openrdf.rio.docformat import DocFormat
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.rio.tupleformat import TupleFormat
from franz.openrdf.sail import AllegroGraphServer
from franz.openrdf.tests.conftest import min_version
from franz.openrdf.tests.tz import MockTimezone
from franz.openrdf.util.contexts import output_to
from franz.openrdf.util.http import normalize_headers
from franz.openrdf.vocabulary import XMLSchema

from franz.openrdf.tests.tests import AG_HOST, AG_PORT, AG_PROXY, CATALOG, STORE, USER, PASSWORD

# Imported to allow mocking
import franz.miniclient.request

common_args = dict(
    host=AG_HOST, port=AG_PORT, catalog=CATALOG,
    user=USER, password=PASSWORD, proxy=AG_PROXY
)


def get_statements(conn):
    """
    Gets all statements as a list of 4-element lists of Value objects,
    sorted lexicographically.
    """
    return sorted([list(s) for s in conn.getStatements()])


def normalize_query_result(result, sort=True):
    """
    Post-process query result to generate a simple, nested list.

    :param result: A QueryResult object.
    :param sort: if True (default) rows will be sorted.
    :return: A list of lists of RDF values.
    """
    normalized = [[row[i] for i in range(len(row))] for row in result]
    return sorted(normalized) if sort else normalized


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

@min_version(6, 6)
def test_export_query_result_tsv(conn):
    base_uri = u'http://franz.com/'
    s = conn.createURI(namespace=base_uri, localname='s')
    p = conn.createURI(namespace=base_uri, localname='p')
    o = conn.createURI(namespace=base_uri, localname='aa \t \n \r \\ bb')
    conn.add(s, p, o)
    conn.commit()
    query_string = "select ?o { ?s ?p ?o }"
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL, query_string)
    out = BytesIO()
    query.evaluate(output=out, output_format=TupleFormat.TSV)
    assert out.getvalue().decode('utf-8') == '?o\n<http://franz.com/aa \\t \\n \\r \\\\ bb>\n'


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

def test_conn_namespace_array(conn):
    ex = conn.namespace('http://franz.com/example/')
    assert ex['bar'] == conn.createURI('http://franz.com/example/bar')

def test_conn_namespace_call(conn):
    ex = conn.namespace('http://franz.com/example/')
    assert ex('baz') == conn.createURI('http://franz.com/example/baz')

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


# A helper function that makes a request and intercepts
# the repl header. It also sorts all the settings
# in the header to make comparisons easier.
def get_repl_header(conn, ex, mocker):
    req = mocker.spy(franz.miniclient.request, 'makeRequest')
    conn.addTriple(ex.s, ex.p, ex.o)
    # not implemented in Python 2...
    # req.assert_called()
    for _args, kwargs in req.call_args_list:
        headers = normalize_headers(kwargs.get('headers', {})) or {}
        return normalize_repl_header(headers.get('x-repl-settings'))


# Sort values in a repl header
def normalize_repl_header(header):
    if header is None:
        return None
    return ' '.join(sorted(header.split()))


def test_default_get_transaction_settings(conn):
    assert conn.getTransactionSettings() is None


def test_set_durability(conn):
    conn.setTransactionSettings(durability=42)
    assert conn.getTransactionSettings().durability == 42


def test_set_durability_does_not_change_latency(conn):
    conn.setTransactionSettings(transaction_latency_count=7)
    conn.setTransactionSettings(durability=42)
    assert conn.getTransactionSettings().transaction_latency_count == 7


def test_set_settings_object(conn):
    conn.setTransactionSettings(durability=42)
    conn.setTransactionSettings(TransactionSettings(transaction_latency_count=7))
    assert conn.getTransactionSettings().transaction_latency_count == 7
    assert conn.getTransactionSettings().durability is None


def test_settings_object_and_kwargs(conn):
    conn.setTransactionSettings(durability=42)
    conn.setTransactionSettings(
        TransactionSettings(transaction_latency_count=7),
        transaction_latency_timeout=2000)
    assert conn.getTransactionSettings().transaction_latency_count == 7
    assert conn.getTransactionSettings().transaction_latency_timeout == 2000
    assert conn.getTransactionSettings().durability is None


def test_settings_object_and_kwargs_overwrite(conn):
    conn.setTransactionSettings(
        TransactionSettings(durability=7), durability=42)
    assert conn.getTransactionSettings().durability == 42


def test_default_transaction_settings(conn, ex, mocker):
    assert get_repl_header(conn, ex, mocker) is None


def test_temporary_settings_object(conn):
    new = TransactionSettings(durability=7)
    with conn.temporaryTransactionSettings(new):
        assert conn.getTransactionSettings() == new


def test_temporary_settings_object_restore(conn):
    old = TransactionSettings(durability=42)
    new = TransactionSettings(durability=7)
    conn.setTransactionSettings(old)
    with conn.temporaryTransactionSettings(new):
        pass
    assert conn.getTransactionSettings() == old


def test_temporary_settings_kwargs(conn):
    with conn.temporaryTransactionSettings(durability=7):
        assert conn.getTransactionSettings().durability == 7


def test_temporary_settings_kwargs_restore(conn):
    old = TransactionSettings(durability=42)
    conn.setTransactionSettings(old)
    with conn.temporaryTransactionSettings(durability=7):
        pass
    assert conn.getTransactionSettings() == old


def test_temporary_settings_kwargs_overwrite(conn):
    old = TransactionSettings(durability=42, transaction_latency_count=10)
    conn.setTransactionSettings(old)
    with conn.temporaryTransactionSettings(durability=7):
        assert conn.getTransactionSettings().durability == 7
        assert conn.getTransactionSettings().transaction_latency_count == 10


def test_temporary_settings_object_and_kwargs_overwrites_all(conn):
    old = TransactionSettings(durability=42)
    new = TransactionSettings()
    conn.setTransactionSettings(old)
    with conn.temporaryTransactionSettings(new, transaction_latency_count=7):
        assert conn.getTransactionSettings().durability is None


def test_temporary_settings_object_and_kwargs(conn):
    old = TransactionSettings(durability=42)
    new = TransactionSettings(durability=7)
    conn.setTransactionSettings(old)
    with conn.temporaryTransactionSettings(new, transaction_latency_count=100):
        assert conn.getTransactionSettings().durability == 7
        assert conn.getTransactionSettings().transaction_latency_count == 100
    assert conn.getTransactionSettings() == old


def test_temporary_settings_nonlocal_return(conn):
    old = TransactionSettings(durability=42)
    new = TransactionSettings(durability=7)
    conn.setTransactionSettings(old)
    while True:
        with conn.temporaryTransactionSettings(new):
            break
    assert conn.getTransactionSettings() == old


def test_set_durability_header(conn, ex, mocker):
    conn.setTransactionSettings(durability=7)
    assert get_repl_header(conn, ex, mocker) == 'durability=7'


def test_set_durability_and_latency_count(conn, ex, mocker):
    conn.setTransactionSettings(durability=7, transaction_latency_count=3)
    settings = get_repl_header(conn, ex, mocker)
    assert settings == 'durability=7 transactionLatencyCount=3'


def test_set_all_transaction_settings(conn, ex, mocker):
    conn.setTransactionSettings(distributed_transaction_timeout=1,
                                durability=2, transaction_latency_count=3,
                                transaction_latency_timeout=4)
    settings = get_repl_header(conn, ex, mocker)
    assert settings == ' '.join(('distributedTransactionTimeout=1',
                                 'durability=2',
                                 'transactionLatencyCount=3',
                                 'transactionLatencyTimeout=4'))


def test_latency_timeout_timedelta(conn, ex, mocker):
    conn.setTransactionSettings(transaction_latency_timeout=timedelta(hours=1))
    assert get_repl_header(conn, ex, mocker) == 'transactionLatencyTimeout=3600'


def test_timeout_timedelta(conn, ex, mocker):
    conn.setTransactionSettings(distributed_transaction_timeout=timedelta(hours=1))
    assert get_repl_header(conn, ex, mocker) == 'distributedTransactionTimeout=3600'


def test_commit_settings(conn, ex, mocker):
    req = mocker.spy(franz.miniclient.request, 'makeRequest')
    conn.addTriple(ex.s, ex.p, ex.o)
    conn.commit(durability='quorum')
    # Not available in Python 2
    # req.assert_called()
    args, kwargs = req.call_args
    headers = normalize_headers(kwargs.get('headers', {})) or {}
    header = normalize_repl_header(headers.get('x-repl-settings'))
    assert header == 'durability=quorum'


def test_define_attribute(conn):
    attr = AttributeDefinition(
        name='test', allowed_values=['a', 'b', 'c'],
        minimum_number=0, maximum_number=1)
    conn.setAttributeDefinition(attr)
    actual = conn.getAttributeDefinition('test')
    assert actual.name == 'test'
    assert sorted(actual.allowed_values) == ['a', 'b', 'c']
    assert actual.ordered is False
    assert actual.minimum_number == 0
    assert actual.maximum_number == 1


def test_get_set_attribute_filter_str(conn, attr):
    conn.setAttributeFilter('(empty triple.test)')
    assert conn.getAttributeFilter() == '(empty triple.test)'


def test_attribute_filter_works(conn, ex, attr):
    conn.addTriple(ex.s1, ex.p1, ex.o1, attributes={'test': 'a'})
    conn.addTriple(ex.s2, ex.p2, ex.o2, attributes={'test': 'b'})
    conn.addTriple(ex.s3, ex.p3, ex.o3, attributes={'test': 'c'})
    conn.setAttributeFilter(TripleAttribute.test == 'b')
    assert get_statements(conn) == [[ex.s2, ex.p2, ex.o2, None]]
    

def test_attribute_filter_works_with_output(conn, ex, attr):
    conn.addTriple(ex.s1, ex.p1, ex.o1, attributes={'test': 'a'})
    conn.addTriple(ex.s2, ex.p2, ex.o2, attributes={'test': 'b'})
    conn.addTriple(ex.s3, ex.p3, ex.o3, attributes={'test': 'c'})
    conn.setAttributeFilter(TripleAttribute.test == 'b')
    buf = io.BytesIO()
    conn.executeTupleQuery('select ?s { ?s ?p ?o }', 
                           output=buf, output_format=TupleFormat.CSV)
    assert buf.getvalue().strip().split() == [b's', b'"ex://s2"']


def test_attribute_filter_with_user_attributes(conn, ex, attr):
    conn.addTriple(ex.s1, ex.p1, ex.o1, attributes={'test': 'a'})
    conn.addTriple(ex.s2, ex.p2, ex.o2, attributes={'test': 'b'})
    conn.addTriple(ex.s3, ex.p3, ex.o3, attributes={'test': 'c'})
    conn.setUserAttributes({'test': 'b'})
    conn.setAttributeFilter(TripleAttribute.test == UserAttribute.test)
    assert get_statements(conn) == [[ex.s2, ex.p2, ex.o2, None]]


@pytest.mark.parametrize("attr_set, expected_text", [
    ('raw-value', '"raw-value"'),
    ('value-with-"-inside', '"value-with-\\"-inside"'),
    (['a', 'list', 'of', 'strings'], '("a" "list" "of" "strings")'),
    (UserAttribute.test, 'user.test'),
    (TripleAttribute.level, 'triple.level')])
def test_attr_set_expr(attr_set, expected_text):
    assert expected_text == attribute_set_to_expr(attr_set)


@pytest.mark.parametrize("attr_filter, expected_text", [
    ('(empty ("raw" "string"))', '(empty ("raw" "string"))'),
    (And('(and)', '(or)'), '(and (and) (or))'),
    (Or('(and)', '(or)'), '(or (and) (or))'),
    (And('(and)',Or()), '(and (and) (or))'),
    (Or(And(), '(or)'), '(or (and) (or))'),
    (Not('(and)'), '(not (and))'),
    (Not(Not(Not(And()))), '(not (not (not (and))))'),
    (Empty([]), '(empty ())'),
    (Empty(TripleAttribute.restricted), '(empty triple.restricted)'),
    (Overlap([], ["x"]), '(overlap () ("x"))'),
    (Overlap(UserAttribute.flags, TripleAttribute.flags),
     '(overlap user.flags triple.flags)'),
    (Subset([], ["x"]), '(subset () ("x"))'),
    (Subset(UserAttribute.flags, TripleAttribute.flags),
     '(subset user.flags triple.flags)'),
    (Superset([], ["x"]), '(superset () ("x"))'),
    (Superset(UserAttribute.flags, TripleAttribute.flags),
     '(superset user.flags triple.flags)'),
    (Equal(["y"], ["x"]), '(equal ("y") ("x"))'),
    (Equal(UserAttribute.color, TripleAttribute.color),
     '(equal user.color triple.color)'),
    (Lt(UserAttribute.level, TripleAttribute.level),
     '(attribute-set< user.level triple.level)'),
    (Le(UserAttribute.level, TripleAttribute.level),
     '(attribute-set<= user.level triple.level)'),
    (Eq(UserAttribute.level, TripleAttribute.level),
     '(attribute-set= user.level triple.level)'),
    (Ge(UserAttribute.level, TripleAttribute.level),
     '(attribute-set>= user.level triple.level)'),
    (Gt(UserAttribute.level, TripleAttribute.level),
     '(attribute-set> user.level triple.level)'),
    (UserAttribute.x < TripleAttribute.x, '(attribute-set< user.x triple.x)'),
    (UserAttribute.x <= TripleAttribute.x, '(attribute-set<= user.x triple.x)'),
    (UserAttribute.x == TripleAttribute.x, '(equal user.x triple.x)'),
    (UserAttribute.x > TripleAttribute.x, '(attribute-set> user.x triple.x)'),
    (UserAttribute.x >= TripleAttribute.x, '(attribute-set>= user.x triple.x)'),
    (UserAttribute.x << TripleAttribute.x, '(subset user.x triple.x)'),
    (UserAttribute.x >> TripleAttribute.x, '(superset user.x triple.x)'),
    ('a' < TripleAttribute.x, '(attribute-set> triple.x "a")'),
    ('a' <= TripleAttribute.x, '(attribute-set>= triple.x "a")'),
    ('a' == TripleAttribute.x, '(equal triple.x "a")'),
    ('a' > TripleAttribute.x, '(attribute-set< triple.x "a")'),
    ('a' >= TripleAttribute.x, '(attribute-set<= triple.x "a")'),
    ('a' << TripleAttribute.x, '(subset "a" triple.x)'),
    ('a' >> TripleAttribute.x, '(superset "a" triple.x)'),
    (Empty([]) & '(or)', '(and (empty ()) (or))'),
    (Empty([]) & Or(), '(and (empty ()) (or))'),
    (Empty([]) | '(and)', '(or (empty ()) (and))'),
    (Empty([]) | And(), '(or (empty ()) (and))'),
    (~Empty([]), '(not (empty ()))')
])
def test_attr_filter_to_expr(attr_filter, expected_text):
    assert expected_text == attribute_filter_to_expr(attr_filter)


def normalize_attributes(d):
    """
    Sort returned attribute values.
    """
    if d is None:
        return None
    r = {}
    for key, value in d.items():
        if isinstance(value, list):
            r[key] = sorted(value)
        else:
            r[key] = value
    return r


def get_triple_attributes(conn, s, p, o, g=None):
    if g is None:
        qt = '''select ?a { 
                   ?a <http://franz.com/ns/allegrograph/6.2.0/attributes> (?s ?p ?o) . 
        }'''
    else:
        qt = '''select ?a { 
                   graph ?g {
                       ?a <http://franz.com/ns/allegrograph/6.2.0/attributes> (?s ?p ?o) .
                   } 
        }'''
    query = conn.prepareTupleQuery(query=qt)
    query.setBinding('s', s)
    query.setBinding('p', p)
    query.setBinding('o', o)
    if g is not None:
        query.setBinding('g', g)
    with query.evaluate() as result:
        return [normalize_attributes(json.loads(b['a'].label or 'null'))
                for b in result
                if b['a'] is not None]


def test_add_data_with_attributes(conn, ex, attr):
    conn.addData('<ex://s> <ex://p> <ex://o> .', attributes={'test': 'c'})
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s, ex.p, ex.o)


def test_add_tuples_with_attributes(conn, ex, attr):
    conn.addTriples([[ex.s1, ex.p1, ex.o1], [ex.s2, ex.p2, ex.o2]],
                    attributes={'test': 'c'})
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s1, ex.p1, ex.o1)
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s2, ex.p2, ex.o2)


def test_add_quints(conn, ex, attr):
    conn.addTriples([[ex.s1, ex.p1, ex.o1, None, {'test': 'a'}],
                    [ex.s2, ex.p2, ex.o2, None, {'test': 'b'}]])
    assert [{'test': 'a'}] == get_triple_attributes(conn, ex.s1, ex.p1, ex.o1)
    assert [{'test': 'b'}] == get_triple_attributes(conn, ex.s2, ex.p2, ex.o2)


def test_add_quints_multiple_values(conn, ex, attr2):
    conn.addTriples([[ex.s1, ex.p1, ex.o1, None, {'test2': ['a', 'b']}],
                     [ex.s2, ex.p2, ex.o2, None, {'test2': ['b', 'c']}]])
    assert [{'test2': ['a', 'b']}] == get_triple_attributes(conn, ex.s1, ex.p1, ex.o1)
    assert [{'test2': ['b', 'c']}] == get_triple_attributes(conn, ex.s2, ex.p2, ex.o2)


def test_add_quints_with_attributes(conn, ex, attr):
    conn.addTriples([[ex.s1, ex.p1, ex.o1, None, {'test': 'a',}],
                     [ex.s2, ex.p2, ex.o2],
                     [ex.s3, ex.p3, ex.o3, None, None],
                     [ex.s4, ex.p4, ex.o4, None, {}]],
                    attributes={'test': 'c'})
    assert [{'test': 'a'}] == get_triple_attributes(conn, ex.s1, ex.p1, ex.o1)
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s2, ex.p2, ex.o2)
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s3, ex.p3, ex.o3)
    assert [] == get_triple_attributes(conn, ex.s4, ex.p4, ex.o4)


def test_set_user_data(server):
    server.setUserData('testKey', 'Hello, "World"!')
    try:
        assert server.getUserData('testKey') == 'Hello, "World"!'
    finally:
        server.deleteUserData('testKey')


def test_user_data_not_found(server):
    assert server.getUserData('nope') is None


def test_del_user_data(server):
    server.setUserData('testKey', 'Hello, "World"!')
    server.deleteUserData('testKey')
    assert server.getUserData('testKey') is None


def test_user_data_fixture(server, user_data):
    user_data['testKey2'] = 'hello!'
    assert server.getUserData('testKey2') == 'hello!'
    del user_data['testKey2']
    assert server.getUserData('testKey2') is None


def test_http_sanity(http_server):
    http_server.publish('/', b'Hello!')
    r = requests.get(http_server.url('/'))
    assert r.text == 'Hello!'


def test_remote_http_sanity(remote_http_server):
    remote_http_server.publish('/', 'Hello!')
    assert remote_http_server.send_request('/') == 'Hello!'


@min_version(6, 5)
def test_add_json_ld(conn, example):
    conn.addData('''{
      "@context": {
        "@vocab": "http://franz.com/example/"
      },
      "@id": "subject",
      "predicate": { "@id": "object" }
    }''', rdf_format=RDFFormat.JSONLD, base_uri=example('').uri)
    assert get_statements(conn) == [[example.subject, example.predicate, example.object, None]]


@min_version(6, 5)
def test_add_json_from_dict(conn, example):
    conn.addData({
      "@context": {
        "@vocab": "http://franz.com/example/"
      },
      "@id": "subject",
      "predicate": {"@id": "object"}
    }, base_uri=example('').uri)
    assert get_statements(conn) == [[example.subject, example.predicate, example.object, None]]


@min_version(6, 5)
def test_add_json_ld_with_context(conn, example):
    conn.addData('''{
      "@id": "subject",
      "predicate": { "@id": "object" }
    }''', rdf_format=RDFFormat.JSONLD, json_ld_context={
        "@vocab": "http://franz.com/example/"
    }, base_uri=example('').uri)
    assert get_statements(conn) == [[example.subject, example.predicate, example.object, None]]


@min_version(6, 5)
def test_add_json_ld_with_external_context(conn, example, remote_http_server):
    url = remote_http_server.publish('/ctx.json', '{"@context": {"@vocab":"http://franz.com/example/"}}')
    conn.addData('''{
      "@id": "subject",
      "predicate": { "@id": "http://franz.com/example/object" }}''',
                 rdf_format=RDFFormat.JSONLD,
                 json_ld_context=url,
                 allow_external_references=True,
                 base_uri=example('').uri)
    assert get_statements(conn) == [[example.subject, example.predicate, example.object, None]]


@min_version(6, 5)
def test_add_json_ld_with_external_context_inside(conn, example, remote_http_server):
    url = remote_http_server.publish('/ctx.json', json.dumps({
       "@context": {
           "predicate": {
               "@id": "http://franz.com/example/predicate",
               "@type": "@id"
           }
       }
    }))
    conn.addData('''{
      "@context": %s,
      "@id": "http://franz.com/example/subject",
      "predicate": { "@id": "http://franz.com/example/object" }}''' % json.dumps(url),
                 rdf_format=RDFFormat.JSONLD,
                 allow_external_references=True)
    assert get_statements(conn) == [[example.subject, example.predicate, example.object, None]]


@min_version(6, 5)
def test_add_json_ld_keep_source(conn):
    src = '''{"ex://p": "o"}'''
    conn.addData(src, rdf_format=RDFFormat.JSONLD, json_ld_store_source=True)
    statements = get_statements(conn)
    assert 2 == len(statements)
    assert statements[0][2].label == src or statements[1][2].label == src

@min_version(6, 6)
def test_add_json_ld_with_root_graph(conn):
    src = '''{"ex://p": "o"}'''
    conn.addData(src, rdf_format=RDFFormat.JSONLD, context=':root')
    statements = get_statements(conn)
    assert 1 == len(statements)
    assert statements[0][0] == statements[0][3]
    

@min_version(6, 5)
def test_add_json_ld_empty_dict(conn):
    conn.addData({})
    assert [] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_simple_dict(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        'p': {'@id': 'o'}
    })
    assert [[example.s, example.p, example.o, None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_uri_key(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        example.p: {'@id': 'o'}
    })
    assert [[example.s, example.p, example.o, None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_uri_values(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': example.s,
        'p': {'@id': example.o}
    })
    assert [[example.s, example.p, example.o, None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_literal_value(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        example.p: Literal('o')
    })
    assert [[example.s, example.p, Literal('o'), None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_typed_literal_value(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        example.p: Literal('oooo', XMLSchema.BASE64BINARY)
    })
    assert [[example.s,
             example.p,
             Literal('oooo', XMLSchema.BASE64BINARY),
             None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_lang_literal_value(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        # A bathtub in Sindarin
        example.p: Literal('🛀', language='sjn')
    })
    assert [[example.s, example.p, Literal('🛀', language='sjn'), None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_integer_literal_value(conn, example):
    conn.addData({
        '@context': {
            '@vocab': 'http://franz.com/example/',
            '@base': 'http://franz.com/example/'
        },
        '@id': 's',
        example.p: Literal(42)
    })
    assert [[example.s, example.p, Literal(42), None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_dict_with_terms(conn, ex):
    conn.addData({
        '@id': ex.s,
        ex.p: ex.o
    })
    assert [[ex.s, ex.p, ex.o, None]] == get_statements(conn)


@min_version(6, 5)
def test_add_json_ld_list(conn, ex):
    conn.addData([{
        '@id': ex.s,
        ex.p: ex.o1
    }, {
        '@id': ex.s,
        ex.p: ex.o2
    }])
    assert [[ex.s, ex.p, ex.o1, None], 
            [ex.s, ex.p, ex.o2, None]] == get_statements(conn)


def test_uri_canonical_true():
    uri1 = URI('ex://test')
    uri2 = URI(namespace='ex://', localname='test')
    assert uri1 is uri2


def test_uri_canonical_false():
    uri1 = URI('ex://test', canonical=False)
    uri2 = URI('ex://test', canonical=False)
    assert uri1 is not uri2


def test_uri_canonicalize():
    uri1 = URI('ex://test', canonical=False)
    uri2 = URI(uri1)
    uri3 = URI('ex://test')
    assert uri2 is not uri1
    assert uri2 is uri3


def test_prolog_default_graph(conn, ex):
    conn.addTriple(ex.s, ex.p, ex.o)
    query = '(select (?s ?p ?o ?g) (q ?s ?p ?o ?g))'
    with conn.executeTupleQuery(query, QueryLanguage.PROLOG) as result:
        bindings = normalize_query_result(result)
    assert bindings == [[ex.s, ex.p, ex.o, None]]

def test_warmup(conn):
    """
    test warmup with various arguments.  we only verify that the HTTP
    call is valid and not that the warmup occured.
    """

    conn.warmup()
    conn.warmup(includeStrings=True)
    conn.warmup(includeStrings=False, includeTriples=False)
    conn.warmup(indices='spogi')
    conn.warmup(indices=['spogi', 'posgi'], includeTriples=True)

def test_sparql_query_metadata_select(conn):
    query = conn.prepareTupleQuery(QueryLanguage.SPARQL,"""
        SELECT (count(*) as ?count) { ?s ?p ?o }""")
    result = query.evaluate()
    md = result.getMetadata()
    assert md
    assert md['time']['total'] > 0

@min_version(7, 1)
def test_query_option_management(conn):
    """
    Test query option management.
    """
    # Default set of query options is empty.
    conn.clearQueryOptions()
    assert len(conn.getQueryOptions()) == 0

    assert_raises(RequestError, lambda n: conn.setQueryOption(n, ''),
                  'unknownQueryOption')

    test_options = {
        'logLineLength': 100,
        'authorizationBasic': '{}:{}'.format(USER, PASSWORD)
    }

    for name, value in iteritems(test_options):
        conn.setQueryOption(name, value)
    assert len(conn.getQueryOptions()) == len(test_options)

    for name, value in iteritems(test_options):
        assert value == conn.getQueryOption(name)

    # Try setting a query option that is already set.
    for name, value in iteritems(test_options):
        conn.setQueryOption(name, value)
    assert len(conn.getQueryOptions()) == len(test_options)

    # Remove one of the options.
    conn.removeQueryOption('logLineLength')
    assert len(conn.getQueryOptions()) == len(test_options) - 1
    assert_raises(RequestError, conn.getQueryOption, 'logLineLength')

    # Test clearing all query options.
    conn.clearQueryOptions()
    assert len(conn.getQueryOptions()) == 0
