###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

"""
Utilities related to context managers.
"""
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
