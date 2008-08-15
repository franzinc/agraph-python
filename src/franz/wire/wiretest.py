

import urllib, urllib2

 
def test1():
    ## net.aserve.client:do-http-request "  
    request = urllib2.Request("http://localhost:8080/sparql")
    request.add_header('accept', 'application/rdf+xml')
#    request.add_header('accept', 'application/rdf+ntriples')
#    request.add_header('accept', 'text/rdf+n3')
#    request.add_header('accept', 'application/x-turtle')
    request.add_header('accept', 'application/x-sparql-compact')
    data = [('query',"SELECT ?s ?o { ?s a ?o . } LIMIT 5")]
    data = urllib.urlencode(data, 1)
    try:
        response = urllib2.urlopen(request, data)
        #response = urllib2.urlopen(request)
        print response.read(), "\n"
    except Exception, e:
        print "HTTP Request failed.", e        
   
if __name__ == '__main__':
    test1()