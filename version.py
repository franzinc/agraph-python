# Get, change, or verify the version number stored in src/franz/_init__.py
# Usage: python version.py <command> [<args...>]
# Commands:
#   get: print the version number to stdout
#   set V: set the version number to V
#   next: Increment the fifth version segment,
#         make sure 'dev0' is at the end.
#   undev: Strip 'dev0' from the version number.
#   verify-dev: Exit with non-zero status if the version
#               does not end in '.dev0'.
#   verify-not-dev: Exit with non-zero status if the 
#                   version ends in .dev0.
import os.path
import re
import sys

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
    "Remove the dev0 suffix from V if it is there."
    if v.endswith('.dev0'):
        return v[:-5]
    return v

def inc(v):
    "Increment the patch level in V."
    segments = [int(s) for s in remove_dev(v).split('.')]
    # Advance the patch level
    segments[2] = segments[2] + 1
    # Construct a new version string in MAJOR.MINOR.PATCH.dev0 format
    return '.'.join(list(map(str, segments[0:3])) + ['dev0'])
    
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
    elif cmd == 'verify-dev':
        if not get_version().endswith('.dev0'):
            sys.stderr.write('Expected a .dev0 version number.\n')
            sys.exit(1)
    elif cmd == 'verify-not-dev':
        if get_version().endswith('.dev0'):
            sys.stderr.write('Expected a release (non .dev0) version number.\n')
            sys.exit(1)
    else:
        sys.stderr.write('Unknown command: %s\n' % cmd)
        sys.exit(1)
        
if __name__ == '__main__':
    main(sys.argv)
