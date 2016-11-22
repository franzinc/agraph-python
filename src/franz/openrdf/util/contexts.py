################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

"""
Utilities related to context managers.
"""
from contextlib import contextmanager
import os
import sys

from past.builtins import basestring


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
        if sys.version_info >= (3, 0):
            yield sys.stdout.buffer
        else:
            yield sys.stdout
    elif isinstance(target, int):
        with os.fdopen(target, 'wb') as out:
            yield out
    elif isinstance(target, basestring):
        with open(target, 'wb') as out:
            yield out
    elif hasattr(target, 'write'):
        # If it quacks like a duck and has a write method...
        yield target
    else:
        raise ValueError('Invalid output specification %s' % target)
