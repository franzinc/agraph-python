################################################################################
# Copyright (c) 2006-2017 Franz Inc.  
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
from datetime import timedelta, tzinfo


class MockTimezone(tzinfo):
    """
    A tzinfo implementation suitable for simple tests.
    """

    def __init__(self, name, utcoffset=timedelta(), dst=timedelta()):
        """
        Initialize a time zone.

        :param name: Display name.
        :type name: str
        :param utcoffset: Offset from the universal time.
                          Can be a timedelta or an int (number of hours).
                          Pass ``None`` to use the resulting tz to mark
                          'naive' time or datetime objects.
        :type utcoffset: timedelta|int|null
        :param dst: Time offset to account for DST.
                    This must be already included in ``utcoffset``.
                    In this test implementation it is constant and does not
                    depend on the date.
                    Can be a timedelta or an int (number of hours).
        :type dst: timedelta|int
        """
        tzinfo.__init__(self)
        self._name = name
        self._utcoffset = to_delta(utcoffset)
        self._dst = to_delta(dst)

    def tzname(self, date_time):
        return self._name

    def utcoffset(self, date_time):
        return self._utcoffset

    def dst(self, date_time):
        return self._dst


def to_delta(value):
    """
    Take either a timedelta or an int (number of hours),
    return a timedelta object.

    If the argument is ``None``, return ``None``.

    :type value: int|timedelta|null
    :rtype: timedelta
    """
    if value is None:
        return None
    elif isinstance(value, timedelta):
        return value
    return timedelta(hours=value)
