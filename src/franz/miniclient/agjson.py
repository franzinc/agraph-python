################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################

import json


class JsonDecodeError(Exception):
    pass


def encode_json(value):
    return json.dumps(value)


def decode_json(text):
    if isinstance(text, bytes):
        text = text.decode("utf-8")
    try:
        return json.loads(text)
    except ValueError:
        raise JsonDecodeError
