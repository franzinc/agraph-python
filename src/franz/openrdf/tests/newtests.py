# -*- coding: utf-8 -*-

################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from datetime import datetime, date, time
from decimal import Decimal

import pytest
import sys

from franz.openrdf.connect import ag_connect
from franz.openrdf.model import Literal
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.sail import AllegroGraphServer
from franz.openrdf.tests.tz import MockTimezone
from franz.openrdf.vocabulary import XMLSchema

from .tests import AG_HOST, AG_PORT, AG_PROXY, CATALOG, STORE, USER, PASSWORD

common_args = dict(
    host=AG_HOST, port=AG_PORT, catalog=CATALOG, user=USER, password=PASSWORD, proxy=AG_PROXY
)


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


def test_server_all_data_in_host():
    server = AllegroGraphServer('https://somehost:4321/and/then/some')
    assert server.url == 'https://somehost:4321/and/then/some'


def test_server_all_defaults():
    server = AllegroGraphServer('somehost')
    assert server.url == 'http://somehost:10035'


def test_server_protocol_as_arg():
    server = AllegroGraphServer('somehost', protocol='https')
    assert server.url == 'https://somehost:10036'


def test_server_override_protocol():
    server = AllegroGraphServer('http://somehost', protocol='https')
    assert server.url == 'https://somehost:10036'


def test_server_port_as_arg():
    server = AllegroGraphServer('somehost', port=4321)
    assert server.url == 'http://somehost:4321'


def test_server_override_port():
    server = AllegroGraphServer('somehost:1234', port=4321)
    assert server.url == 'http://somehost:4321'


def test_server_https_if_cainfo():
    server = AllegroGraphServer('somehost', cainfo='/path/to/ca/bundle')
    assert server.url == 'https://somehost:10036'


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
    actual_format, actual_compression = RDFFormat.rdf_format_for_file_name(filename)
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
