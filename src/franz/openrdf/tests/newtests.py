###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v0.html
###############################################################################
import pytest

from franz.openrdf.connect import ag_connect
from franz.openrdf.rio.rdfformat import RDFFormat
from franz.openrdf.sail import AllegroGraphServer

from .tests import AG_HOST, AG_PORT, CATALOG, STORE, USER, PASSWORD

common_args = dict(
    host=AG_HOST, port=AG_PORT, catalog=CATALOG, user=USER, password=PASSWORD
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
