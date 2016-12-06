# The version number must follow these rules:
#   - When the server is released, a client with exactly the same version number
#     should be released.
#   - Bugfixes should be released as consecutive post-releases,
#     that is versions of the form X.Y.Z.postN, where X.Y.Z is
#     the AG version number and N increases with each fix.
#   - Code from the development branch may be released any time
#     with a version of the form X.Y.ZrcN (rc = release candidate).
#
# When this file is committed to git the version should look like this:
#   - In any branch that has already been released:  X.Y.Z
#     AG and Python client versions should be the same.
#   - In a 'stable' branch: X.Y.ZpostN, where X.Y.Z is the PREVIOUS
#     version of AG.
#   - In the development branch: X.Y.ZpreN.
#
# The full range of valid version numbers is described here:
# https://www.python.org/dev/peps/pep-0440/
__version__ = u'6.1.4'
