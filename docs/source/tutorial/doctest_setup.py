# This document contains definitions of functions and variables used 
# in multiple tutorial examples. Each function is actually passed to Sphinx 
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
# Each function is surrounded by START-<NAME> and END-<NAME> markers to allow 
# easy inclusions through the literalinclude directive.

# Py2/3 compatibility
from __future__ import print_function

# BEGIN-VARIABLES
import os
from getpass import getpass

AGRAPH_HOST = os.environ.get('AGRAPH_HOST', 'localhost')
AGRAPH_PORT = os.environ.get('AGRAPH_PORT', '10035')
AGRAPH_CATALOG = os.environ.get('AGRAPH_CATALOG', '') 
AGRAPH_REPOSITORY = \
   os.environ.get('AGRAPH_REPOSITORY', 'pythontutorial')
AGRAPH_USER = os.environ.get('AGRAPH_USER', 'test')
AGRAPH_PASSWORD = os.environ.get('AGRAPH_PASSWORD')
if AGRAPH_PASSWORD is None:
   AGRAPH_PASSWORD = getpass('Password for %s:' % AGRAPH_USER)
DATA_DIR = os.path.abspath(os.environ.get('DATA_DIR', 'data'))
# END-VARIABLES

# BEGIN-CONNECT
from franz.openrdf.connect import ag_connect

def connect():
   return ag_connect(AGRAPH_REPOSITORY,
                     host=AGRAPH_HOST, port=AGRAPH_PORT,
                     catalog=AGRAPH_CATALOG,
                     user=AGRAPH_USER, password=AGRAPH_PASSWORD,
                     create=True, clear=True)
# END-CONNECT

# BEGIN-ADD-BOB-AND-ALICE
def add_bob_and_alice(conn):
   conn.addData("""
       @base <http://example.org/> .

       <people/alice> a <ontology/Person> ;
                      <ontology/name> "Alice" .
       <people/bob> a <ontology/Person> ;
                    <ontology/name> "Bob" .
   """)
# END-ADD-BOB-AND-ALICE
