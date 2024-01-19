from contextlib import contextmanager

from franz.openrdf.repository.repositoryconnection import RepositoryConnection


class TriplesBuffer:
    def __init__(self, conn: RepositoryConnection, limit: int = 1000):
        """
        Cosntruct an instance of TriplesBuffer.

        Parameters
        ----------

            conn : RepositoryConnection
                an instance of RepositoryConnection
            limit : int
                size limit for the triples buffer
        """

        self._conn = conn
        self._limit = limit
        self._buffer = []

    def append(self, triple):
        """
        Add a triple to the buffer.
        """

        if len(self) >= self._limit:
            self.flush()
        self._buffer.append(triple)

    def flush(self):
        """
        Flush all the pending triples in the buffer by calling "addTriples",
        and then reset the buffer.
        """

        if len(self) > 0:
            self._conn.addTriples(self._buffer)
            self._buffer = []

    def __len__(self):
        return len(self._buffer)


@contextmanager
def buffered_triples(conn: RepositoryConnection, limit: int = 1000):
    """
    Construct an instance of TriplesBuffer that automatically flushes before
    the end of with statement.
    """

    buf = TriplesBuffer(conn, limit)
    try:
        yield buf
    finally:
        buf.flush()
