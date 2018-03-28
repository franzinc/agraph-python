"""
Types and utilities related to transaction settings.
"""
from collections import namedtuple
try:
    from itertools import zip_longest  # Py3
except ImportError:
    from itertools import izip_longest as zip_longest  # Py2


# The subclass is created to allow us to provide a docstring for Sphinx.
class TransactionSettings(namedtuple('TransactionSettings', (
        'distributed_transaction_timeout', 'durability',
        'transaction_latency_count', 'transaction_latency_timeout'))):
    """
    A named tuple encapsulating a set of distributed transaction parameters.

    Each parameter here can be set to ``None``, in which case the actual value
    of that setting will be determined by the server.

    When creating a new instance not all arguments have to be specified,
    the default value for all settings is ``None``.

    Attributes:
        durability: (int|str):
            The durability is a positive integer value that specifies how many
            instances must have a commit ingested in order for that commit to be
            considered durable. The count includes the instance that made
            the commit.

            A durability setting of 1 means that when an instance makes a commit that
            commit is immediately considered durable before even being sent to any other
            instance (the commit will still be sent to the other instances after it's
            considered durable).

            A value that equals the total number of nodes in the cluster means that every
            instance must have ingested the commit before it's considered durable.
            If one or more instances are stopped at the moment then the commit will not
            become durable until the stopped instances are restarted.

            Three symbolic values (strings) may be used instead of numbers:
                - 'min', which simply means 1.
                - 'max', which means that all instances in the cluster must
                  ingest a commit before it is considered durable.
                - 'quorum', which means that more than a half of instances
                  must ingest a commit for it to be considered durable.

        distributed_transaction_timeout (int|datetime.timedelta):
            Use this setting to specify how long a commit call will wait for
            the commit to become durable. It's a non-negative integer
            (number of seconds).

            If the durability is greater than one then the committing process
            has to wait for acknowledgements from the other servers that the
            transaction was committed. The committing process returns to the
            caller when the durability has been reached or the distributed
            transaction timeout seconds has passed, whichever comes first.

            When the commit returns the caller does not know if durability has
            been reached.

        transaction_latency_count (int):
            Number of commits. Use this setting to limit the number of non-durable
            (pending) commits that can be active on the cluster. If this limit is
            reached all new commits will signal an error (and have to be retried).

            When a commit is done the committing process tries to wait until the
            commit is durable but if that takes too long (see
            distributed_transaction_timeout) then commit will return with the
            system still working on making that transaction durable.

            If the latency count is 4 then even if the last four commits are not yet
            durable it is possible to do one more commit. But if there are five
            pending transactions then any attempt to commit will result in an error.

            Another example: If you set the latency count to zero then each commit must
            be durable before the next commit can be ingested.

        transaction_latency_timeout (int|datetime.timedelta):
            Use this setting to specify how long a commit operation should wait for the
            transaction_Latency_count to be satisfied before throwing an error.

    """
    __slots__ = ()

    def __new__(cls, *args, **kwargs):
        """
        A constructor that provides default values.

        by default a named tuple requires all fields to be provided, this
        constructor will treat all missing values as ``None``.
        """
        params = dict(zip_longest(TransactionSettings._fields, args))
        params.update(kwargs)
        return super(TransactionSettings, cls).__new__(cls, **params)


DEFAULT_TRANSACTION_SETTINGS = TransactionSettings()