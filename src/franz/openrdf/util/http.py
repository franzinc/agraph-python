# HTTP utilities that do not depend on a particular backend


def normalize_headers(headers):
    """
    Create a dictionary of headers from:
       * A list of curl-style headers
       * None
       * a dictionary (return a *copy*).

    :param headers: List or dict of header (may also be None).
    :type headers: Iterable[string] | dict[string, string] | None
    :return: A dictionary of headers suitable for requests.
    :rtype: dict[string,string]
    """
    if headers is None:
        return {}
    elif isinstance(headers, dict):
        return headers.copy()
    else:
        # Assume curl-style sequence of strings
        result = {}
        for line in headers:
            key, sep, value = line.partition(':')
            if sep is None:
                raise Exception("Internal error - invalid header line (%s)" % line)
            result[key.strip().lower()] = value.strip()
        return result


def merge_headers(headers, extra_headers):
    """
    Merge two sets of HTTP headers.

    Each set can be either a dictionary, a list of strings or ``None``.
    The returned set is either a dictionary or ``None``.

    note that the returned set might be one of the original sets,
    so it should not be modified later.

    :param headers: The first set of headers.
    :type headers: Iterable[string] | dict[string, string] | None
    :param extra_headers: The second set of headers.
    :type extra_headers: Iterable[string] | dict[string, string] | None
    :return: A merged set of headers.
    :rtype: Iterable[string] | dict[string, string] | None
    """
    if not extra_headers:
        return headers
    if not headers:
        return extra_headers
    return normalize_headers(headers).update(normalize_headers(extra_headers))
