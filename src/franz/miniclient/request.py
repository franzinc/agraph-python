################################################################################
# Copyright (c) 2006-2017 Franz Inc.
# All rights reserved. This program and the accompanying materials are
# made available under the terms of the MIT License which accompanies
# this distribution, and is available at http://opensource.org/licenses/MIT
################################################################################
import array
import re
import sys
from io import StringIO
from itertools import islice
from urllib.parse import quote

from franz.miniclient.agjson import decode_json
from franz.miniclient.backends.requests import makeRequest
from franz.openrdf.util.http import merge_headers
from franz.openrdf.util.strings import to_native_string


def jsonRequest(
    obj,
    method,
    url,
    body=None,
    content_type="application/x-www-form-urlencoded",
    callback=None,
    accept=None,
    headers=None,
):
    """
    Create a request that expects a JSON response.

    The response can optionally be saved to a file-like object if
    the connection object has the _saveFile and _saveAccept attributes.

    Instead of being returned the response might be passed to a callback function.

    Raise an exception if the returned status is not in the 2XX range.

    :param obj: Service object with connection information (e.g. credentials).
    :type obj: franz.openrdf.miniclient.repository.Service
    :param method: Request method (``"GET"``, ``"POST"``, ...).
    :type method: string
    :param url: Target address
    :type url: string
    :param body: Request body (for PUT/POST requests) or query string, optional.
    :type body: str|bytes|file
    :param accept: Value of the accept header (default: ``"application/json"``)
    :type accept: string
    :param content_type: MIME type of the request body, optional.
    :type content_type: string
    :param headers: Either a dictionary mapping headers to values or
                    a list of strings that will be included in the request's headers.
    :type headers: Iterable[string] | dict[string, string] | None
    :return: Status code and response body, unless callback is specified (in that case None is returned).
    :param callback: A callback function that will be called for each response chunk (optional).
                     The return value should be either None or the number of bytes
                     received, anything else will cause the request to be aborted.
    :type callback: (bytestring) -> int

    :return: A parsed JSON response or ``None`` if the response was saved to a file or processed by a callback.
    :rtype: dict|string|int|float|None
    """
    if accept is None:
        accept = "application/json"

    # If there is a _saveFile and _saveAccept, they override the arguments
    if hasattr(obj, "_saveFile") and hasattr(obj, "_saveAccept"):
        accept = obj._saveAccept
        callback = obj._saveFile.write

    headers = merge_headers(obj.getHeaders(), headers)
    if callback is None:
        status, body = makeRequest(
            obj, method, url, body, accept, content_type, headers=headers
        )
        if status == 204:
            body = decode_json("{}")
            return body
        elif status == 200:
            if accept in (
                "application/json",
                "text/integer",
                "application/x-quints+json",
            ):
                body = decode_json(body)
            return body
        else:
            raise RequestError(status, body)
    else:

        def raiseErr(status, message):
            raise RequestError(status, message)

        makeRequest(
            obj,
            method,
            url,
            body,
            accept,
            content_type,
            callback=callback,
            errCallback=raiseErr,
            headers=headers,
        )


def nullRequest(
    obj,
    method,
    url,
    body=None,
    content_type="application/x-www-form-urlencoded",
    content_encoding=None,
):
    """
    Create a request that expects an empty response body.

    Raise an exception if the returned status is not in the 2XX range.

    :param obj: Service object with connection information (e.g. credentials).
    :type obj: franz.openrdf.miniclient.repository.Service
    :param method: Request method (``"GET"``, ``"POST"``, ...).
    :type method: string
    :param url: Target address
    :type url: string
    :param body: Request body (for PUT/POST requests) or query string, optional.
    :type body: str|bytes|file
    :param content_type: MIME type of the request body, optional.
    :type content_type: string
    """
    headers = None
    if content_encoding is not None:
        headers = ["Content-Encoding: " + content_encoding]
    status, body = makeRequest(
        obj,
        method,
        url,
        body,
        "application/json",
        content_type,
        headers=merge_headers(obj.getHeaders(), headers),
    )
    if status < 200 or status > 204:
        raise RequestError(status, body)


if sys.version_info[0] == 2:
    # Workaround for a bug in python-future
    def ibytes(x):
        """
        Construct a bytes object from a sequence or iterator over integers.
        In Python 3, bytes() can do that, but python-future does not have
        that capability.
        """
        if not hasattr(x, "__len__"):
            return bytes(list(x))
        return bytes(x)

else:
    ibytes = bytes


def mk_unicode(text):
    if not isinstance(text, str):
        return str(text, "utf-8")
    return text


class RequestError(Exception):
    code = None

    def __init__(self, status, message):
        # Why the [----] did anynone think it's ok for a client
        # library to just happily write stuff to stdout?!?!
        # I can't fix it now, because our Lisp query cancelling test
        # depends on this nonsensical behavior.
        print(status, message)
        self.status = status
        if status == 400:
            match = re.match("([A-Z ]+): (.*)", message)
            if match:
                self.code = match.group(1)
                message = match.group(2)
        self.message = message

    def __str__(self):
        return "Server returned %s: %s" % (self.status, self.message)


def urlenc(**args):
    buf = StringIO()

    def enc(name, val):
        if buf.tell():
            buf.write("&")
        buf.write(quote(to_native_string(name)))
        buf.write("=")
        buf.write(quote(to_native_string(val)))

    def encval(name, val):
        # we represent a hypen in a parameter name with double underscores
        name = name.replace("__", "-")
        if val is None:
            pass
        elif isinstance(val, bool):
            enc(name, (val and "true") or "false")
        elif isinstance(val, int):
            encval(name, "%d" % val)
        elif isinstance(val, float):
            encval(name, "%g" % val)
        elif isinstance(val, list) or isinstance(val, tuple):
            for elt in val:
                encval(name, elt)
        elif isinstance(val, (str, bytes)):
            enc(name, to_native_string(val))
        else:
            enc(name, to_native_string(str(val)))

    for arg_name, value in args.items():
        encval(arg_name, value)
    return buf.getvalue()


class SerialConstants:
    SO_VECTOR = 1
    SO_STRING = 5
    SO_NULL = 7
    SO_LIST = 8
    SO_POS_INTEGER = 9
    SO_END_OF_ITEMS = 10
    SO_NEG_INTEGER = 11
    SO_BYTEVECTOR = 15


def serialize(obj):
    def serialize_int(i):
        # make sure i is non negative
        i = abs(i)

        def int_bytes(i):
            rest = True
            while rest:
                lower = i & 0x7F
                rest = i >> 7
                yield lower | (0x80 if rest else 0)
                i = rest

        return ibytes(int_bytes(i))

    if obj is None:
        return bytes([SerialConstants.SO_NULL])

    if isinstance(obj, str):
        return b"".join(
            [
                bytes([SerialConstants.SO_STRING]),
                serialize_int(len(obj)),
                bytes(obj, "utf-8"),
            ]
        )

    if isinstance(obj, int):
        return b"".join(
            [
                bytes([SerialConstants.SO_POS_INTEGER])
                if obj >= 0
                else bytes([SerialConstants.SO_NEG_INTEGER]),
                serialize_int(obj),
            ]
        )

    try:
        # Byte vector
        if obj.typecode == b"b":
            return b"".join(
                [
                    bytes([SerialConstants.SO_BYTEVECTOR]),
                    serialize_int(len(obj)),
                    obj.tostring(),
                ]
            )
    except:
        pass

    try:
        iobj = iter(obj)
        return b"".join(
            [
                bytes([SerialConstants.SO_VECTOR]),
                serialize_int(len(obj)),
                b"".join([serialize(elem) for elem in iobj]),
            ]
        )
    except:
        pass

    raise TypeError("cannot serialize object of type %s" % type(obj))


def deserialize(string):
    def posInteger(chars):
        result = shift = 0

        # Set value to get into the loop the first time
        value = 0x80
        while value & 0x80:
            value = next(chars)
            result += (value & 0x7F) << shift
            shift += 7

        return result

    if isinstance(string, str):
        string = bytes(string)
    chars = iter(string)
    value = next(chars)

    if value == SerialConstants.SO_BYTEVECTOR:
        length = posInteger(chars)
        return array.array(b"b", [ord(next(chars)) for i in range(length)])

    if value == SerialConstants.SO_VECTOR or value == SerialConstants.SO_LIST:
        length = posInteger(chars)
        return [deserialize(chars) for i in range(length)]

    if value == SerialConstants.SO_STRING:
        length = posInteger(chars)
        return str(ibytes(islice(chars, 0, length)), "utf-8")

    if value == SerialConstants.SO_POS_INTEGER:
        return posInteger(chars)

    if value == SerialConstants.SO_NEG_INTEGER:
        return -posInteger(chars)

    if value == SerialConstants.SO_NULL:
        return None

    if value == SerialConstants.SO_END_OF_ITEMS:
        return None

    raise ValueError("bad code found by deserializer: %d" % value)


def encode(string):
    def convert(string):
        codes = encode.codes
        state = rem = 0

        for byte in bytes(string):
            if state == 0:
                yield codes[byte & 0x3F]
                rem = (byte >> 6) & 0x3
                state = 1
            elif state == 1:
                yield codes[((byte & 0xF) << 2) | rem]
                rem = (byte >> 4) & 0xF
                state = 2
            else:
                yield codes[((byte & 0x3) << 4) | rem]
                yield codes[((byte >> 2) & 0x3F)]
                state = 0

        if state:
            yield codes[rem]

    return ibytes(convert(string))


encode.codes = bytes(
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*+"
)


def decode(string):
    def convert(string):
        codes = decode.codes
        state = rem = 0

        if isinstance(string, str):
            string = bytes(string, "utf-8")
        else:
            string = bytes(string)

        for byte in string:
            byte = codes[byte]

            if state == 0:
                rem = byte
                state = 1
            elif state == 1:
                yield rem | ((byte & 0x3) << 6)
                rem = byte >> 2
                state = 2
            elif state == 2:
                yield rem | ((byte & 0xF) << 4)
                rem = byte >> 4
                state = 3
            else:
                yield rem | (byte << 2)
                state = 0

    return ibytes(convert(string))


decode.codes = [
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    62,
    63,
    0,
    0,
    0,
    0,
    52,
    53,
    54,
    55,
    56,
    57,
    58,
    59,
    60,
    61,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    0,
    0,
    0,
    0,
    0,
    0,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    40,
    41,
    42,
    43,
    44,
    45,
    46,
    47,
    48,
    49,
    50,
    51,
]
