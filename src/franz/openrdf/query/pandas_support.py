import pandas
from six.moves import zip
from collections import OrderedDict

from franz.openrdf.model import Literal


def term_to_pandas(term):
    if term is None:
        return None
    if isinstance(term, Literal):
        return term.toPython()
    else:
        # URI or blank node
        return str(term)


def rows_to_pandas(rows, column_names):
    columns = [[] for _ in column_names]
    for row in rows:
        for idx, term in enumerate(row):
            columns[idx].append(term_to_pandas(term))
    return pandas.DataFrame(OrderedDict(zip(column_names, columns)))
