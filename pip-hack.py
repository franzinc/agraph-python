# This script outputs its arguments, removing any instances
# of --trusted-host (and the associated value) if it detects
# that pip is too old to support that argument.
# It must work in any version of Python from 2.6 up, including 3.X.

import pip
import pipes
import sys

version = tuple(int(component) for component in pip.__version__.split('.'))
args = sys.argv[1:]
if version < (6,):
    curr = 0
    while curr < len(args):
        if args[curr] == '--trusted-host':
            del args[curr]
            if curr < len(args):
                del args[curr]
        elif args[curr].startswith('--trusted-host='):
            del args[curr]
        else:
            curr += 1

sys.stdout.write(' '.join(map(pipes.quote, args)))
