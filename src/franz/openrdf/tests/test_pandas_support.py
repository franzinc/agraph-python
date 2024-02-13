from franz.openrdf.repository.repositoryconnection import RepositoryConnection
from franz.openrdf.tests.conftest import conn

TEST_DATA = """
prefix ex: <ex://>
ex:c2000 ex:year 2000; ex:cheese 29.8 .
ex:c2001 ex:year 2001; ex:cheese 30.1 .
ex:c2002 ex:year 2002; ex:cheese 30.5 .
ex:c2003 ex:year 2003; ex:cheese 30.6 .
ex:c2004 ex:year 2004; ex:cheese 31.3 .
ex:c2005 ex:year 2005; ex:cheese 31.7 .
ex:c2006 ex:year 2006; ex:cheese 32.6 .
ex:c2007 ex:year 2007; ex:cheese 33.1 .
ex:c2008 ex:year 2008; ex:cheese 32.7 .
ex:c2009 ex:year 2009; ex:cheese 32.8 .
ex:d2000 ex:year 2000; ex:doctorates 480 .
ex:d2001 ex:year 2001; ex:doctorates 501 .
ex:d2002 ex:year 2002; ex:doctorates 540 .
ex:d2003 ex:year 2003; ex:doctorates 552 .
ex:d2004 ex:year 2004; ex:doctorates 547 .
ex:d2005 ex:year 2005; ex:doctorates 622 .
ex:d2006 ex:year 2006; ex:doctorates 655 .
ex:d2007 ex:year 2007; ex:doctorates 701 .
ex:d2008 ex:year 2008; ex:doctorates 712 .
ex:d2009 ex:year 2009; ex:doctorates 708 ."""


def test_pandas_support_repositoryresult(conn: RepositoryConnection):
    conn.addData(TEST_DATA)

    df = conn.getStatements(predicate=conn.createURI("ex://year")).toPandas()
    assert len(df) == 20

    df = conn.getStatements(predicate=conn.createURI("ex://cheese")).toPandas()
    assert len(df) == 10
    assert sorted([float(x) for x in df["o"]]) == sorted(
        [
            29.8,
            30.1,
            30.5,
            30.6,
            31.3,
            31.7,
            32.6,
            33.1,
            32.7,
            32.8,
        ]
    )

    df = conn.getStatements(predicate=conn.createURI("ex://doctorates")).toPandas()
    assert len(df) == 10
    assert sorted(list(df["o"])) == sorted(
        [
            480,
            501,
            540,
            552,
            547,
            622,
            655,
            701,
            712,
            708,
        ]
    )


def test_pandas_support_queryresult(conn: RepositoryConnection):
    conn.addData(TEST_DATA)
    query = """
prefix ex: <ex://>
select ?year ?cheese ?doctorates {
    _:b1 ex:year ?year ; ex:cheese ?cheese .
    _:b2 ex:year ?year ; ex:doctorates ?doctorates .
} order by ?year"""

    with conn.executeTupleQuery(query) as result:
        df = result.toPandas()
        assert len(df) == 10
        assert list(df.columns) == ["year", "cheese", "doctorates"]
        assert list(df["year"]) == [
            2000,
            2001,
            2002,
            2003,
            2004,
            2005,
            2006,
            2007,
            2008,
            2009,
        ]
        assert [float(x) for x in df["cheese"]] == [
            29.8,
            30.1,
            30.5,
            30.6,
            31.3,
            31.7,
            32.6,
            33.1,
            32.7,
            32.8,
        ]
        assert list(df["doctorates"]) == [
            480,
            501,
            540,
            552,
            547,
            622,
            655,
            701,
            712,
            708,
        ]
