################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Utilities related to context managers.
"""
import doctest
import io
import os
import sys
from contextlib import contextmanager


@contextmanager
def wrap_context(value):
    """
    Wrap a no-op context around an arbitrary value, so that it can be
    used in a ``with`` statement.

    :param value: Value to be returned by the context manager.
    :return: A context manager wrapping ``value``.
    """
    yield value


@contextmanager
def output_to(target):
    """
    Returns a file-like object constructed from target.

    Target can be:

        - None: the returned stream will ignore all data
        - True: data will be written to stdout.
        - a string: a file will be opened (in binary mode)
        - a file-like object: will be returned as is.
        - an int - a file descriptor.

    :param target: File path, None or a file-like object.
    """
    if target is None:
        # TODO: consider replacing with an object with empty methods
        target = os.devnull

    # Note that True is also an int (and it equals 1 = stdout),
    # but we give it a special treatment because our doctests
    # are not smart enough to capture things written to an fd.
    if target is True:
        # Make sure that the thing we return supports binary I/O
        if hasattr(sys.stdout, "buffer"):
            yield sys.stdout.buffer
        # Doctest checker only accepts strings, not bytes...
        elif isinstance(sys.stdout, doctest._SpoofOut):
            yield BinWrapper(sys.stdout)
        # io.TextIOBase only accepts strings, not bytes...
        elif isinstance(sys.stdout, io.TextIOBase):
            yield BinWrapper(sys.stdout)
        else:
            yield sys.stdout
    elif isinstance(target, int):
        with os.fdopen(target, "wb") as out:
            yield out
    elif isinstance(target, (str, bytes)):
        with open(target, "wb") as out:
            yield out
    elif hasattr(target, "write"):
        # If it quacks like a duck and has a write method...
        if hasattr(target, "buffer"):
            yield target.buffer
        else:
            yield target
    else:
        raise ValueError("Invalid output specification %s" % target)


# Used to convert text output streams to binary UTF-8 streams
class BinWrapper:
    def __init__(self, target):
        self.target = target

    def write(self, byte_string):
        self.target.write(byte_string.decode("utf-8"))
