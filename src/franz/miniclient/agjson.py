###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

from __future__ import print_function, unicode_literals

import sys

# JSON abstraction layer

try:
    import simplejson as json
except ImportError:
    import json


class JsonDecodeError(Exception):
    pass


def encode_json(value):
    return json.dumps(value)


def decode_json(text):
    # JSON on Py3 insists on getting Unicode strings
    if sys.version_info[0] > 2 and isinstance(text, bytes):
        text = text.decode('utf-8')
    try:
        return json.loads(text)
    except ValueError:
        raise JsonDecodeError

