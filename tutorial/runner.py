from tutorial_examples import main

# Runs the tutorial with an excepthook that causes
# uncaught exceptions in all threads to kill the
# whole process and exit with a non-zero code.

import os
import sys
import threading
import time
import traceback

def handler(exc_type, value, tb):
    # Print stacktrace
    traceback.print_exception(exc_type, value, tb)
    # Kill all threads immediately.
    os._exit(1)

sys.excepthook = handler

def install_thread_excepthook():
    """
    Replaces 'run' in threads to make them use sys.excepthook.
    The replacement is done by monkey-patching Thread.__init__,
    This makes sure that the replaced method is used in subclasses
    of Thread.
    """
    init_old = threading.Thread.__init__
    def init(self, *args, **kwargs):
        init_old(self, *args, **kwargs)
        run_old = self.run
        def run_with_except_hook(*args, **kw):
            try:
                run_old(*args, **kw)
            except (KeyboardInterrupt, SystemExit):
                raise
            except:
                sys.excepthook(*sys.exc_info())
        self.run = run_with_except_hook
    threading.Thread.__init__ = init


install_thread_excepthook()
main()
