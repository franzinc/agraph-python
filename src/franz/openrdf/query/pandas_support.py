from collections import OrderedDict

import pandas as pd

from franz.openrdf.model import Literal


def term_to_pandas(term):
    if term is None:
        return None
    if isinstance(term, Literal):
        return term.toPython()
    else:
        # URI or blank node
        return str(term)


def rows_to_pandas(rows, column_names) -> pd.DataFrame:
    columns = [[] for _ in column_names]
    for row in rows:
        for idx, term in enumerate(row):
            columns[idx].append(term_to_pandas(term))
    return pd.DataFrame(OrderedDict(zip(column_names, columns)))
