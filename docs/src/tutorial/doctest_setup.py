# This document contains definitions of functions and variables used 
# in multiple tutorial examples. Each code fragment is actually passed to Sphinx
# twice:
#   - This whole file is imported in a hidden, global setup block
#   - The example in which the function is defined included its code from this 
#     file.
#
# The reason for all this is that definitions in one document, even if contained 
# in a 'testsetup:: *' block, are not visible in other documents. The global 
# setup block is the only way of sharing definitions, but we still want them
# presented in the appropriate part of the tutorial.
#
# Each fragment is surrounded by START-<NAME> and END-<NAME> markers to allow
# easy inclusions through the literalinclude directive.

# BEGIN-CONNECT
from franz.openrdf.connect import ag_connect

conn = ag_connect('python-tutorial', create=True, clear=True)
# END-CONNECT

