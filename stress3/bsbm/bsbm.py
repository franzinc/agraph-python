#!/usr/bin/env python
import base64, http.client, locale, os, subprocess, sys, urllib.request, urllib.parse, urllib.error

class Defaults:
    # Goal store size
    SIZE = (10**8)

    # OPEN OR CREATE
    OPEN = False

    # Full text index
    FTI = False

    # Warmups
    WARMUPS = 50

    # Runs
    RUNS = 500

# The program options
OPT = None
ORIG_SIZE_ARG = '100m'

LOCALHOST = 'localhost'
AG_HOST = os.environ.get('AGRAPH_HOST', LOCALHOST)
AG_PORT = int(os.environ.get('AGRAPH_PORT', '10035'))
AG_USER = os.environ.get('AGRAPH_USER', 'test')
AG_PASSWORD = os.environ.get('AGRAPH_PASSWORD', 'xyzzy')
PROG = sys.argv[0]

def trace(formatter, *values):
    if values:
        formatter = formatter % values
    print(formatter)
    sys.stdout.flush()

headers = {'Content-type': 'application/x-www-form-urlencoded',
          'Authorization': 'Basic %s' % base64.b64encode('%s:%s' % (AG_USER, AG_PASSWORD)).strip(),
          'Accept': 'text/plain'}

def cpus():
    # Linux
    try:
        res = open('/proc/cpuinfo').read().count('processor\t:')
        if res > 0:
            return res
    except IOError:
        pass

# Use the administrative user to ensure there is an anonymous user
def ensure_anonymous(name):
    conn = http.client.HTTPConnection(AG_HOST, AG_PORT)
    conn.request('GET', '/users', headers=headers)
    users = conn.getresponse().read()

    if 'anonymous' not in users:
        conn.request('PUT', '/users/anonymous', headers=headers)
        conn.getresponse().read()

    conn.request('PUT',
        '/users/anonymous/access?read=true&catalog=/&repository=%s' % name,
        headers=headers)
    conn.getresponse().read()
    conn.close()

QUERY6_FTI = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX bsbm: <http://www4.wiwiss.fu-berlin.de/bizer/bsbm/v01/vocabulary/>

SELECT ?product ?label
WHERE {
    ?product fti:match "%word1%" .
    ?product rdfs:label ?label .
    ?product rdf:type bsbm:Product .
}
"""

QUERY6_ORIG = """
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX bsbm: <http://www4.wiwiss.fu-berlin.de/bizer/bsbm/v01/vocabulary/>

SELECT ?product ?label
WHERE {
	?product rdfs:label ?label .
    ?product rdf:type bsbm:Product .
	FILTER regex(?label, "%word1%")
}
"""

def setup_store(name):
    # Create the store and add the text index
    query6 = QUERY6_ORIG
    conn = http.client.HTTPConnection(AG_HOST, AG_PORT)
    conn.request('PUT', '/repositories/%s' % name, headers=headers)
    trace('Create store: %s', conn.getresponse().read())

    if (OPT.FTI):
        conn.request('PUT', ('/repositories/%s/freetext/indices/fti' % name) +
            '?predicate=%3Chttp%3A//www.w3.org/2000/01/rdf-schema%23label%3E',
            headers=headers)
        trace('Create freetext index: %s', conn.getresponse().read())
        conn.close()
        query6 = QUERY6_FTI

    query = open('queries/query6.txt', 'w')
    query.write(query6)
    query.close()

def triples_to_product_count(triples):
    # doesn't seem quite linear (!?)
    if triples < 100000:
        return triples * 91 / 36946

    if triples <= 1000000:
        return triples * 2785 / 1000000

    if triples <= 25000000:
	    return triples * 70812 / 25000000

    if triples <= 100000000:
        return triples * 284826 / 100000000

    return triples * 570000 / 200000000

def run_cmd(cmd):
    try:
        trace(cmd)
        retcode = subprocess.call(cmd, shell=True)
        if retcode < 0:
            trace("Child was terminated by signal %d", -retcode)
        else:
            trace("Child returned %d", retcode)
    except OSError as e:
        trace("Execution failed: %s", e)

def split_file(name, pattern, lines):
    file_num = 0
    line_num = 0
    orig = open(name, 'r')
    output = None
    for line in orig:
        if line_num == 0:
            output = open(pattern % file_num, 'w')
            file_num += 1
        print(line, end=' ', file=output)
        line_num += 1
        if line_num == lines:
            output.close()
            output = None
            line_num = 0

    if output:
        output.close()
    orig.close()

def store_exists(name): 
    conn = http.client.HTTPConnection(AG_HOST, AG_PORT)
    conn.request('GET', '/repositories', headers=headers)
    stores = conn.getresponse().read()
    conn.close()

    return ('id: %s\n' % name) in stores

def create_store(triples, name):
    # -pc == product count (91 ==> 50k triples ? more like 37k)
    # -fn == filename 
    # -fc == add extra rdf:type statements
    # -dir == output for test data

    # Check to see if store already exists
    if store_exists(name) and OPT.OPEN:
        print('Using existing store %s.' % name)
        return

    prod_count = triples_to_product_count(triples)
    source = './dataset-%s.nt' % prod_count

    if os.path.exists(source):
        print('Using existing dataset file %s.' % source)
    else:
        print('Generating dataset file %s.' % source)
        cmd = ('java -Xms512m -Xmx512m '
	        '-cp bin:lib/ssj.jar benchmark.generator.Generator '
	        '-pc %d '
	        '-s nt '
	        '-fc '
	        '-fn dataset-%d '
	        '-dir test-data-%d' % (prod_count, prod_count, prod_count))
        run_cmd(cmd)

    print('Splitting source file...')
    split_file(source, 'bsbm-load-%03d.nt', triples/(cpus()-1 or 1))
    setup_store(name)
    run_cmd('agload -i ntriples --port %d %s bsbm-load-\*' % (AG_PORT, name))
    run_cmd('rm -f bsbm-load-*.nt')

def run_queries(triples, name, warmups=1, runs=1, clients=1, seed=0, reduced=False):
    prod_count = triples_to_product_count(triples)
    cmd = ('java -cp bin:lib/ssj.jar:lib/log4j-1.2.12.jar:lib/jdom.jar '
        'benchmark.testdriver.TestDriver '
        '-w %d '
        '-runs %d '
        '-seed %d '
        '-idir ./test-data-%d '
        '-mt %d '
        '-o %s-%s-mix-results-%d-%d-%d-%d.xml '
        'http://localhost:%d/repositories/%s ' % (warmups, runs,
            seed, prod_count, clients, name, (reduced and 'reduced') or 'full',
            warmups, runs, clients, seed, AG_PORT, name))

    ignoreQueries = open('ignoreQueries.txt', 'w')
    if reduced:
        ignoreQueries.write('5\n6\n') 
    ignoreQueries.close()
    run_cmd(cmd)

def main():
    # Open a connection on the shared port
    name = 'bsbm-%s' % ORIG_SIZE_ARG
    size = OPT.SIZE
    print('cd ./bsbmtools')
    os.chdir('./bsbmtools')
    create_store(size, name)
    ensure_anonymous(name)
    run_queries(size, name, OPT.WARMUPS, OPT.RUNS, 1, 808080, False)
    run_queries(size, name, OPT.WARMUPS, OPT.RUNS, 4, 863528, False)
    run_queries(size, name, OPT.WARMUPS, OPT.RUNS, 1, 808080, True)
    run_queries(size, name, OPT.WARMUPS, OPT.RUNS, 4, 863528, True)

if __name__ == '__main__':
    from copy import copy
    from optparse import OptionParser, Option, OptionValueError

    locale.setlocale(locale.LC_ALL, '')

    usage = 'Usage: %prog [options]\n\n' \
        'Runs a Berlin Benchmark script similar to:\n\n' \
        'http://www4.wiwiss.fu-berlin.de/bizer/BerlinSPARQLBenchmark/results/V5/index.html\n\n' \
        'Environment Variables Consulted:\n' \
        'PATH - for finding agload\n' \
        'AGRAPH_HOST [default=localhost]\n' \
        'AGRAPH_PORT [default=10035]\n' \
        'AGRAPH_USER [default=test]\n' \
        'AGRAPH_PASSWORD [default=xyzzy]'

    def check_human_size(option, opt, value):
        global ORIG_SIZE_ARG
        try:
            ORIG_SIZE_ARG = value
            if value[-1] == 'm':
                value = locale.atof(value[:-1])*10**6
            elif value[-1] == 'b':
                value = locale.atof(value[:-1])*10**9
            elif value[-1] == 't':
                value = locale.atof(value[:-1])*10**12
            else:
                value = locale.atoi(value)

            return int(value)
        except ValueError:
            raise OptionValueError(
                "option %s: invalid human-readable size value: %r" % (opt, value))

    class BSBMOptions(Option):
        TYPES = Option.TYPES + ('human_size','profiler')
        TYPE_CHECKER = copy(Option.TYPE_CHECKER)
        TYPE_CHECKER['human_size'] = check_human_size
    
    parser = OptionParser(option_class=BSBMOptions, usage=usage, version="%prog 1.0")
    parser.add_option('-s', '--size', default=Defaults.SIZE,
        type='human_size', dest='SIZE', metavar='SIZE',
        help='SIZE triple limit for BSBM load (e.g. 10,000, 100m, 2b, 1.5t) [default=%default]')
    parser.add_option('-o', '--open', default=Defaults.OPEN,
        dest='OPEN', metavar='OPEN', action="store_true",
        help='OPEN the existing BSBM instead of LOADING it [default=RENEW]')
    parser.add_option('-f', '--fti', default=Defaults.FTI,
        dest='FTI', metavar='FTI', action="store_true",
        help='Create the full text index for modified Query 6 [default='
            'standard BSBM query 6]')
    parser.add_option('-w', '--warmups', default=Defaults.WARMUPS,
        dest='WARMUPS', metavar='WARMUPS', type=int,
        help='The number of WARMUPS to run [default=%default]')
    parser.add_option('-r', '--runs', default=Defaults.RUNS,
        dest='RUNS', metavar='RUNS', type=int,
        help='The number of query RUNS [default=%default]')


    options, args = parser.parse_args()
    OPT = options
    del args
    main()
