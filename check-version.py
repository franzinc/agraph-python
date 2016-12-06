# This script compares the AG version number passed as a parameter
# with the client version stored in src/franz/__init__.py.
# The rules of the comparison are as follows:
#   - If the version number in __init__.py is a post-release then
#     one component of the AG version must be greater by one than
#     the corresponding Python version component. Components to the
#     left must be equal, the ones to the right must be zeros in
#     the AG version and can have any value in the Python version.
#   - If the Python version is not a post-release, its base version
#     must match that of the AG server.
#
# Matching result is returned as the exit code of the process.
# 0 means success while 1 means there is a mismatch.
#
# A message containing both versions is also printed to stderr.
#
# Usage: python check_version AG_VERSION
#
# Note: this must work in all versions of Python 2.6+, including 3.X.
# No external dependencies are allowed.
import collections
import os.path
import re
import sys

# Note that this only matches the kind of version numbers that
# we use, not the full range supported by PEP-440
python_version_re = re.compile(
    r'''^(?P<base>\d+(?:\.\d+)*)     # Base version (X.Y.Z...)
        (?:rc(?P<pre>\d+))           # Optional pre (rcN)
        ?(?:\.post(?P<post>\d+))?$   # Optional post: .postN''',
    re.VERBOSE | re.IGNORECASE)


# base is a sequence of integers, pre and post are integers or None
Version = collections.namedtuple('Version', ('base', 'pre', 'post'))


def parse_version(version):
    """
    Parse a version number.

    Works for both AG and Python module (i.e. client) versions.

    :param version: Module version.
    :type version: str
    :return: A parsed version.
    :rtype: Version
    """
    m = python_version_re.match(version)
    if m is None:
        raise Exception('Invalid version number: %s' % version)
    py_base = tuple(int(x) for x in m.group('base').split('.'))
    py_pre = int(m.group('pre')) if m.group('pre') is not None else None
    py_post = int(m.group('post')) if m.group('post') is not None else None
    return Version(base=py_base, pre=py_pre, post=py_post)


def versions_match(client_version, ag_version):
    """
    Check if the version numbers match.

    See the header of this file for definition of matching versions.

    :param client_version: Python module version (parsed).
    :type client_version: Version
    :param ag_version: AllegroGraph server version (parsed)
    :type ag_version: Version
    :return: True if versions match, False otherwise.
    """
    client_base = list(client_version.base)
    ag_base = list(ag_version.base)

    # Pad to make the lengths equal.
    # Note that trailing zeros are not significant in PEP-440
    client_base += [0] * max(0, len(ag_base) - len(client_base))
    ag_base += [0] * max(0, len(client_base) - len(ag_base))

    if client_version.post is not None:
        # Post release - check if AG_VERSION could possibly be the next release
        found_diff = False
        for ag, py in zip(ag_base, client_base):
            if found_diff and ag != 0:
                # Non-zero component found to the right of the first difference
                return False
            elif not found_diff and ag != py:
                if ag != py + 1:
                    # Difference is not one = not the next version.
                    return False
                found_diff = True
        # Return false if the versions were identical
        return found_diff
    else:
        # Pre or regular release - the versions must match exactly
        return client_base == ag_base


def main(args):
    base_dir = os.path.dirname(os.path.abspath(args[0]))
    sys.path.insert(0, os.path.join(base_dir, 'src'))
    version_module = __import__('franz')
    raw_client_version = version_module.__version__
    raw_ag_version = args[1]
    client_version = parse_version(raw_client_version)
    ag_version = parse_version(raw_ag_version)
    if not versions_match(client_version, ag_version):
        sys.stderr.write("ERR: Mismatched Server/Client version numbers:\n")
        sys.stderr.write("ERR:   Server version: %s\n" % raw_ag_version)
        sys.stderr.write("ERR:   Client version: %s\n" % raw_client_version)
        sys.stderr.write("ERR: See comments in src/franz/__init__.py and "
                         "adjust the version number accordingly.")
        sys.exit(1)
    else:
        if client_version.post is None:
            kind = 'POST-RELEASE'
        elif client_version.pre is not None:
            kind = 'PRE-RELEASE (RC)'
        else:
            kind = 'RELEASE'
        sys.stderr.write('Preparing %s distribution.\n' % kind)
        sys.stderr.write("AG version: %s\n" % raw_ag_version)
        sys.stderr.write("Python module version: %s\n" % raw_client_version)

if __name__ == '__main__':
    main(sys.argv)
