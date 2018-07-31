# -*- coding: utf-8 -*-
import gzip
import os
import tempfile

import pytest

from franz.openrdf.model import Literal, URI
from franz.openrdf.repository.repositoryconnection import DocumentKey
from franz.openrdf.rio.docformat import DocFormat
from franz.openrdf.tests.conftest import min_version
from franz.openrdf.tests.newtests import get_statements, get_triple_attributes

import csv

# Python 2 does not have csv.unix_dialect
if not hasattr(csv, 'unix_dialect'):
    class unix_dialect(csv.Dialect):
        delimiter = ','
        quotechar = '"'
        doublequote = True
        skipinitialspace = False
        lineterminator = '\n'
        quoting = csv.QUOTE_ALL
    csv.unix_dialect = unix_dialect
    csv.register_dialect("unix", unix_dialect)

# Skip all tests on older server versions
pytestmark = min_version(6, 5)


def uri_forms(uri):
    """
    Used to test that all forms of a URI are accepted.
    """
    return [uri, '<%s>' % uri, URI(uri)]


def test_add_doc_data_empty(conn):
    conn.addDocumentData('{}')
    assert get_statements(conn) == []


def test_add_doc_data_empty_dict(conn):
    conn.addDocumentData({})
    assert get_statements(conn) == []


def test_add_doc_data_empty_list(conn):
    conn.addDocumentData([])
    assert get_statements(conn) == []


def test_add_doc_data_csv(conn):
    conn.addDocumentData('a,b,c\ndd,ee,ff', DocFormat.CSV)
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('dd'), None],
                          [subject, ns.b, Literal('ee'), None],
                          [subject, ns.c, Literal('ff'), None]]


def test_add_doc_data_csv_columns(conn):
    conn.addDocumentData('dd,ee,ff', DocFormat.CSV, csv_columns=['a', 'b', 'c'])
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('dd'), None],
                          [subject, ns.b, Literal('ee'), None],
                          [subject, ns.c, Literal('ff'), None]]


@pytest.mark.parametrize('uri', uri_forms('ex://'))
def test_add_doc_data_csv_base(conn, ex, uri):
    conn.addDocumentData('a,b,c\ndd,ee,ff', DocFormat.CSV, base=uri)
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    assert statements == [[subject, ex.a, Literal('dd'), None],
                          [subject, ex.b, Literal('ee'), None],
                          [subject, ex.c, Literal('ff'), None]]


@pytest.mark.parametrize('uri', uri_forms('ex://s'))
def test_add_doc_data_subject(conn, ex, uri):
    conn.addDocumentData('a,b,c\ndd,ee,ff', DocFormat.CSV, subject=uri)
    statements = get_statements(conn)
    assert 3 == len(statements)
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[ex.s, ns.a, Literal('dd'), None],
                          [ex.s, ns.b, Literal('ee'), None],
                          [ex.s, ns.c, Literal('ff'), None]]


def test_add_doc_data_subject_template(conn, ex):
    conn.addDocumentData('a,b,c\ndd,ee,ff', DocFormat.CSV, subject='ex://$a')
    statements = get_statements(conn)
    assert 3 == len(statements)
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[ex.dd, ns.a, Literal('dd'), None],
                          [ex.dd, ns.b, Literal('ee'), None],
                          [ex.dd, ns.c, Literal('ff'), None]]


def test_add_doc_data_csv_base_and_subject(conn, ex):
    conn.addDocumentData('a,b,c\ndd,ee,ff', DocFormat.CSV, base='ex://', subject='ex://$a')
    assert get_statements(conn) == [[ex.dd, ex.a, Literal('dd'), None],
                                    [ex.dd, ex.b, Literal('ee'), None],
                                    [ex.dd, ex.c, Literal('ff'), None]]


def test_add_doc_data_rules(conn, ex):
    conn.addData('''
        @prefix ldm: <http://franz.com/ns/allegrograph/6.4/load-meta#> .
        ldm:myrules ldm:id "ex://test${x}" ;
                    ldm:prefix "ex://" .''')
    conn.addDocumentData({'x': "abc"}, rules='myrules')
    statements = [stmt for stmt in get_statements(conn)
                  if 'load-meta' not in str(stmt[0])]
    assert statements == [[ex.testabc, ex.x, Literal('abc'), None]]


@pytest.mark.parametrize('pref1, pref2', zip(uri_forms('p1://'), uri_forms('p2://')))
def test_add_doc_data_prefix(conn, pref1, pref2):
    p1 = conn.namespace('p1://')
    p2 = conn.namespace('p2://')
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb'
    }, prefix={
        'a': pref1,
        'b': pref2
    })
    statements = get_statements(conn)
    assert len(statements) == 2
    assert statements == [[statements[0][0], p1.a, Literal('aa'), None],
                          [statements[1][0], p2.b, Literal('bb'), None]]


@pytest.mark.parametrize('pref1, pref2', zip(uri_forms('p1://'), uri_forms('p2://')))
def test_add_doc_data_prefix_in_keys(conn, pref1, pref2):
    p1 = conn.namespace('p1://')
    p2 = conn.namespace('p2://')
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb'
    }, keys={
        'a': DocumentKey(prefix=pref1),
        'b': DocumentKey(prefix=pref2)
    })
    statements = get_statements(conn)
    assert len(statements) == 2
    assert statements == [[statements[0][0], p1.a, Literal('aa'), None],
                          [statements[1][0], p2.b, Literal('bb'), None]]


@pytest.mark.parametrize('pref1, pref2', zip(uri_forms('p1://'), uri_forms('p2://')))
def test_add_doc_data_prefix_override(conn, pref1, pref2):
    p2 = conn.namespace('p2://')
    conn.addDocumentData({
        'a': 'aa'
    }, keys={
        'a': DocumentKey(prefix=pref1),
    }, prefix={
        'a': pref2
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements == [[statements[0][0], p2.a, Literal('aa'), None]]


def test_add_doc_data_rename_uri(conn, ex):
    conn.addDocumentData({
        'a': 'aa'
    }, rename={
        'a': ex.p
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][1] == ex.p


def test_add_doc_data_rename_key(conn):
    conn.addDocumentData({
        'a': 'aa'
    }, rename={
        'a': 'p'
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][1].getLocalName() == 'p'


@pytest.mark.parametrize('uri', uri_forms('ex://t'))
def test_add_doc_data_rdf_type(conn, ex, uri):
    conn.addDocumentData({
        'a': 'aa'
    }, rdf_type={
        'a': uri
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][2].datatype == ex.t


def test_add_doc_data_rdf_type_uri(conn, ex):
    conn.addDocumentData({
        'a': 'ex://aa'
    }, rdf_type={
        'a': 'uri'
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][2] == ex.aa


def test_add_doc_data_lang(conn):
    conn.addDocumentData({
        'a': 'aa'
    }, lang={
        'a': 'sjn'
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][2].language == 'sjn'


def test_add_doc_data_skip(conn):
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb'
    }, skip=['a'])
    assert len(get_statements(conn)) == 1


def test_add_doc_data_skip_key(conn):
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb'
    }, keys={
        'b': DocumentKey(skip=True)
    })
    assert len(get_statements(conn)) == 1


def test_add_doc_data_skip_both(conn):
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb',
        'c': 'cc'
    }, skip=[
        'a'
    ], keys={
        'b': DocumentKey(skip=True)
    })
    assert len(get_statements(conn)) == 1


def test_add_doc_data_transform_built_in(conn):
    conn.addDocumentData({
        'a': 'aa'
    }, transform={
        'a': 'string-capitalize'
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][2] == Literal('Aa')


def test_add_doc_data_transform_template(conn):
    conn.addDocumentData({
        'a': 'XY'
    }, transform={
        'a': 'test-$a'
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][2] == Literal('test-XY')


@pytest.mark.parametrize('uri', uri_forms('ex://g'))
def test_add_doc_data_graph(conn, ex, uri):
    conn.addDocumentData({
        'a': 'aa'
    }, graph={
        'a': uri
    })
    statements = get_statements(conn)
    assert len(statements) == 1
    assert statements[0][3] == ex.g


def test_add_doc_data_store_source(conn):
    # Empty dict will not be stored at all, so we need at least one key
    src = '{"a":"bb"}'
    conn.addDocumentData(src, json_store_source=True)
    for _s, _p, o, _g in get_statements(conn):
        if o == Literal(src):
            break
    else:
        pytest.fail('Source document not found.')


def test_add_doc_data_csv_separator(conn):
    conn.addDocumentData('a:b:c\nxx:yy:zz', DocFormat.CSV, csv_separator=':')
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('xx'), None],
                          [subject, ns.b, Literal('yy'), None],
                          [subject, ns.c, Literal('zz'), None]]


def test_add_doc_data_csv_quote(conn):
    conn.addDocumentData('a,b,c\nxx,?y y?,zz', DocFormat.CSV, csv_quote='?')
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('xx'), None],
                          [subject, ns.b, Literal('y y'), None],
                          [subject, ns.c, Literal('zz'), None]]


def test_add_doc_data_csv_whitespace(conn):
    conn.addDocumentData('a,b,c\nxx,????yy?,zz', DocFormat.CSV, csv_whitespace='?')
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('xx'), None],
                          [subject, ns.b, Literal('yy'), None],
                          [subject, ns.c, Literal('zz'), None]]


def test_add_doc_data_csv_double_quote(conn):
    conn.addDocumentData('a,b,c\nxx,"y""y",zz', DocFormat.CSV, csv_double_quote=True)
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('xx'), None],
                          [subject, ns.b, Literal('y"y'), None],
                          [subject, ns.c, Literal('zz'), None]]


def test_add_doc_data_csv_escape(conn):
    conn.addDocumentData('a,b,c\nxx,"y?"y",zz', DocFormat.CSV, csv_escape='?')
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('xx'), None],
                          [subject, ns.b, Literal('y"y'), None],
                          [subject, ns.c, Literal('zz'), None]]


@pytest.mark.parametrize('excel_tab',
                         ['excel-tab', csv.excel_tab])
def test_add_doc_data_csv_dialect(conn, excel_tab):
    conn.addDocumentData('a\tb\tc\r\n"x""x"\t yy\tzz', DocFormat.CSV,
                         csv_dialect=excel_tab)
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('x"x'), None],
                          [subject, ns.b, Literal(' yy'), None],
                          [subject, ns.c, Literal('zz'), None]]


class UnixWithoutDoubleQuote(csv.unix_dialect):
    doublequote = False


@pytest.mark.parametrize('doc, arg, value, dialect', [
    ('a;b;c\n"x""x"; yy;zz', 'csv_separator', ';', 'unix'),
    ('a,b,c\nx"x,? yy?,zz', 'csv_quote', '?', 'unix'),
    ('a,b,c\n"x""x", yy,???zz', 'csv_whitespace', '?', 'unix'),
    ('a,b,c\n"x""x", yy,zz', 'csv_double_quote', True, UnixWithoutDoubleQuote),
    ('a,b,c\n"x?"x", yy,zz', 'csv_escape', '?', 'unix'),
])
def test_add_doc_data_csv_override_dialect(conn, doc, arg, value, dialect):
    conn.addDocumentData(doc, DocFormat.CSV, csv_dialect=dialect,
                         **{arg: value})
    statements = get_statements(conn)
    assert 3 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('x"x'), None],
                          [subject, ns.b, Literal(' yy'), None],
                          [subject, ns.c, Literal('zz'), None]]


@pytest.mark.parametrize('uri', uri_forms('ex://g'))
def test_add_doc_data_context(conn, ex, uri):
    conn.addDocumentData({
        'a': 'aa',
        'b': 'bb'
    }, context=uri)
    statements = get_statements(conn)
    assert 2 == len(statements)
    subject = statements[0][0]
    ns = conn.namespace(statements[0][1].getNamespace())
    assert statements == [[subject, ns.a, Literal('aa'), ex.g],
                          [subject, ns.b, Literal('bb'), ex.g]]


def test_add_doc_data_commit(conn):
    before = conn.getGeneration()
    conn.addDocumentData([{'a': 'aa'}] * 100, commit=1)
    after = conn.getGeneration()
    assert 100 == conn.size()
    # AG seems to be doing more commits than necessary
    assert 100 <= after - before


def test_add_doc_data_attributes(conn, ex, attr):
    conn.addDocumentData({'p': 'oo'},
                         base=ex(''), subject=ex.s, attributes={'test': 'c'})
    assert [{'test': 'c'}] == get_triple_attributes(conn, ex.s, ex.p, Literal('oo'))


def test_add_doc_encoding(conn, after):
    f = tempfile.NamedTemporaryFile(suffix='.csv', delete=False)
    filename = f.name
    after.append(lambda: os.remove(filename))
    f.write(u'p\nदुप🛁'.encode('utf-16le'))
    f.close()

    # This is completely broken... it expects an external format name here
    conn.addDocumentFile(filename, DocFormat.CSV, encoding='unicode')
    statements = get_statements(conn)
    assert 1 == len(statements)
    assert statements[0][2] == Literal(u"दुप🛁")


def test_add_doc_content_encoding(conn, after):
    f = tempfile.NamedTemporaryFile(suffix='.json.gz', delete=False)
    filename = f.name
    after.append(lambda: os.remove(filename))
    with gzip.GzipFile('data.json', 'wb', fileobj=f) as gz:
        gz.write('{"p":"oo"}'.encode('utf-8'))
    f.close()

    conn.addDocumentFile(filename, DocFormat.JSON, content_encoding='gzip')
    statements = get_statements(conn)
    assert 1 == len(statements)
    assert statements[0][2] == Literal("oo")
