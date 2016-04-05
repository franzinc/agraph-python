###############################################################################
# Copyright (c) 2006-2016 Franz Inc.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the Eclipse Public License v1.0
# which accompanies this distribution, and is available at
# http://www.eclipse.org/legal/epl-v10.html
###############################################################################

"""Dump stack traces of running Python processes.

To enable live stack trace dumping in a Python process, simply import
tipper when the program starts.

To dump the stack trace of each thread, send the SIGUSR1 signal to its
process. The stack trace will be dumped to following location:

$TMPDIR/tipper-[unix timestamp]-[parent pid]-[pid].log
"""

from __future__ import with_statement

import datetime
import inspect
import linecache
import os
import pprint
import signal
import sys
import tempfile
import time

from future.utils import iteritems

try:
    import threading
except ImportError:
    import dummy_threading as threading

__all__ = []

class SafePrettyPrinter(pprint.PrettyPrinter, object):
    def format(self, obj, context, maxlevels, level):
        try:
            return super(SafePrettyPrinter, self).format(
                obj, context, maxlevels, level)
        except Exception:
            return object.__repr__(obj)[:-1] + ' (bad repr)>'

def spformat(obj, depth=None):
    return SafePrettyPrinter(indent=1, width=76, depth=depth).pformat(obj)

def formatvalue(v):
    s = spformat(v, depth=1).replace('\n', '')
    if len(s) > 250:
        s = object.__repr__(v)[:-1] + ' (really long repr)>'
    return '=' + s

def stack_with_locals(f):
    if hasattr(sys, 'tracebacklimit'):
        limit = sys.tracebacklimit
    else:
        limit = None

    frames = []
    n = 0
    while f is not None and (limit is None or n < limit):
        lineno, co = f.f_lineno, f.f_code
        name, filename = co.co_name, co.co_filename
        args = inspect.getargvalues(f)

        linecache.checkcache(filename)
        line = linecache.getline(filename, lineno, f.f_globals)
        if line:
            line = line.strip()
        else:
            line = None

        frames.append((filename, lineno, name, line, f.f_locals, args))
        f = f.f_back
        n += 1
        frames.reverse()

    out = []
    for filename, lineno, name, line, localvars, args in frames:
        out.append('  File "%s", line %d, in %s' % (filename, lineno, name))
        if line:
            out.append('    %s' % line.strip())

        args = inspect.formatargvalues(*args, formatvalue=formatvalue)
        out.append('\n      Arguments: %s%s' % (name, args))

        if localvars:
            out.append('      Local variables:\n')
            try:
                reprs = spformat(localvars)
            except Exception:
                reprs = "failed to format local variables"
            out += ['      ' + l for l in reprs.splitlines()]
            out.append('')
    return '\n'.join(out)

def dump(signum, sigframe):
    """Signal handler that dumps the traceback of the given frame"""
    try:
        try:
            threadid = threading.currentThread().ident
        except Exception:
            threadid = None

        frames = sys._current_frames()
        try:
            t, ppid, pid = time.time(), os.getppid(), os.getpid()
            fn = 'tipper-%s-%s-%s.log' % (t, ppid, pid)
            header = """Date: %s
Parent process ID: %s
Process ID: %s
""" % (datetime.datetime.fromtimestamp(t), ppid, pid)

            with open(os.path.join(tempfile.gettempdir(), fn), 'w') as f:
                f.write(header)
                for ident, frame in iteritems(frames):
                    if ident == threadid:
                        frame = frame.f_back
                    f.write('\nTraceback (thread %s):\n\n' % ident)
                    f.write(stack_with_locals(frame))
        finally:
            del frames
    except Exception:
        pass

signal.signal(signal.SIGUSR1, dump)
