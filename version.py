# Get or change the version number stored in src/franz/_init__.py
# Usage: python version.py <command> [<args...>]
# Commands:
#   get: print the version number to stdout
#   set V: set the version number to V
#   next: Increment the fifth version segment,
#         make sure 'dev' is at the end.
#   undev: Strip 'dev' from the version number.
#   check V: where V is the AG server version.
#            Checksif the server and client versions
#            match. If not, print an error message
#            and exit with a non-zero status.
#   verify-dev: Exit with non-zero status if the version
#               does not end in '.dev'.
#   verify-not-dev: Exit with non-zero status if the 
#                   version ends in .dev.
import os.path
import re
import sys

# Version numbers consist of the AG version number padded with 0s
# to this length and a single client-release segment.
AG_VERSION_LENGTH = 4

BASE_DIR = os.path.dirname(os.path.realpath(__file__)) if '__file__' in globals() else os.getcwd()

INIT = os.path.join(BASE_DIR, 'src', 'franz', '__init__.py')

pattern = re.compile(r'__version__\s*=\s*.*$', re.M)

def get_version():
    "Read the version number from sources."
    sys.path.insert(0, os.path.join(BASE_DIR, 'src'))
    version_module = __import__('franz')
    return version_module.__version__

def set_version(v):
    "Change the version number to V."
    old = get_version()
    sys.stderr.write('%s -> %s\n' % (old, v))
    with open(INIT, 'r+') as f:
        text = f.read()
        text = pattern.sub("__version__ = %r" % v, text)
        f.seek(0)
        f.truncate()
        f.write(text)

def remove_dev(v):
    "Remove the dev suffix from V if it is there."
    if v.endswith('.dev'):
        return v[:-4]
    return v

def inc(v):
    "Increment the client release segment in V."
    segments = [int(s) for s in remove_dev(v).split('.')]
    segments += [0] * max(0, AG_VERSION_LENGTH - len(segments) + 1)
    segments[AG_VERSION_LENGTH] += 1
    return '.'.join(list(map(str, segments)) + ['dev'])
    
def check_ag_version(ag_version):
    """
    Check if the current version number matches the AG version.  Print
    an error message and return False if it does not.  Otherwise
    return True.

    First AG_VERSION_LENGTH segments must match, the dev suffix is allowed
    in the client version.
    """
    ag_segments = ag_version.strip().split('.')
    if len(ag_segments) > AG_VERSION_LENGTH:
        sys.stderr.write('AG version (%s) is too long. ' % ag_version + 
                         'It can have at most %d segments.\n' % AG_VERSION_LENGTH)
        return True
    # get the client verion, cut off .dev if it is there
    client_version = remove_dev(get_version())
    client_segments = client_version.strip().split('.')

    # pad the AG version if necessary
    ag_segments += ['0'] * (AG_VERSION_LENGTH - len(ag_segments))
    
    # pad/trim the client version to match 
    client_segments += ['0'] * (AG_VERSION_LENGTH - len(client_segments))
    client_segments = client_segments[:AG_VERSION_LENGTH]

    # Compare
    if ag_segments != client_segments:
        sys.stderr.write('Python client version does not match the server version.\n')
        sys.stderr.write('   Server version: %s\n' % ag_version)
        sys.stderr.write('   Client version: %s\n' % client_version)
        sys.stderr.write('Consider adjusting the client version in src/franz/__init__.py\n')
        return False
    return True


def main(args):
    cmd = args[1] if len(args) > 1 else 'get'
    if cmd == 'get':
        sys.stdout.write(get_version())
    elif cmd == 'set':
        set_version(args[2])
    elif cmd == 'next':
        set_version(inc(get_version()))
    elif cmd == 'undev':
        set_version(remove_dev(get_version()))
    elif cmd == 'check':
        if not check_ag_version(args[2]):
            sys.exit(1)
    elif cmd == 'verify-dev':
        if not get_version().endswith('.dev'):
            sys.stderr.write('Expected a .dev version number.\n')
            sys.exit(1)
    elif cmd == 'verify-not-dev':
        if get_version().endswith('.dev'):
            sys.stderr.write('Expected a release (non .dev) version number.\n')
            sys.exit(1)
    else:
        sys.stderr.write('Unknown command: %s\n' % cmd)
        sys.exit(1)
        
if __name__ == '__main__':
    main(sys.argv)
