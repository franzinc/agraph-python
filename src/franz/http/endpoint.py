


import urllib2

class DoHTTP:
    def __init__(self, host, port, verbose=False):
        self.host = host
        self.port = port
        self.verbose = verbose
        self.current_repository = None
        
    def _make_url(self, command, quote=True):
        if quote:
            command = urllib2.quote(command)
        return "http://%s:%s/sesame/%s" % (self.host, self.port, command)
    
    def do_http_request(self, command, accept=None, data=None):
        request = urllib2.Request(self._make_url(command, quote=False))
        if self.verbose:
            print "Request: ", self._make_url(command, quote=False)  ##, "\n         ", self._make_url(command)
        try:
            
#            request.addheaders = [('User-Agent','Mozilla/5.0 (Windows; U; WindowsNT 5.1; en-US; rv:1.8.1.14) Gecko/20080404 Firefox/2.0.0.14'),
#                                  ('Accept','text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5')]
            
            request.add_header('accept', 'application/rdf+xml')
            request.add_header('accept', 'application/rdf+ntriples')
            request.add_header('accept', 'text/rdf+n3')
            request.add_header('accept', 'application/x-turtle')
            #request.add_header('accept', 'application/sparql-results+xml')                                   
            if data:
                response = urllib2.urlopen(request, data)
            else:
                response = urllib2.urlopen(request)
        except Exception, e:
            if self.verbose:
                print "Request failed because ", e, "\n"
            return None                
        if self.verbose:
            print "Response: ", response.code
        print response.read(), "\n"
        return response.read()
    
    def do_http_repository_request(self, command, repository=None, accept=None):
        repository = repository or self.current_repository
        command = "repositories/%s/%s" % (repository, command)
        return self.do_http_request(command, accept=accept)
    
    def get_repositories(self):
        return self.do_http_request('repositories')
    
    def open_repository(self, name):
        response = self.do_http_request("repositories?id=%s&if-exists=open" % name)
        self.current_repository = name
        return response
    
    def get_size(self):
        return self.do_http_repository_request("size")

    def get_namespaces(self):
        return self.do_http_repository_request("namespaces")

    def get_all_statements(self):
        return self.do_http_repository_request("statements")
    
    def get_statements(self, subject, predicate, object, repository=None):
        terms = []
        s = "subj=%s" % urllib2.quote(subject) if subject else ''
        if s: terms.append(s)
        p = "pred=%s" % urllib2.quote(predicate) if predicate else ''
        if p: terms.append(p)
        o = "obj=%s" % urllib2.quote(object) if object else ''
        if o: terms.append(o)
        args = '&'.join(terms)        
        query = """statements?%s""" % args                                             
        return self.do_http_repository_request(query, accept="application/rdf+xml")
    
#(do-http-request
# (concatenate
#  'string
#  "http://localhost:8123/sesame/repositories/NEWSTORE2/statements?"
#  "subj=" (uriencode-string "\"person1\"")
#  "&pred=" (uriencode-string "\"firstname\"")
#  "&obj=" (uriencode-string "\"steve\""))
# :method :post :content "")
#;; result should be 204 (OK, empty reply)



#        
#        
#        (do-http-request
# "http://localhost:8123/sesame/repositories?id=NEWSTORE3&if-exists=supersede"
# :method :post)
#;; result should be 204 (OK, empty reply)
    
    

###

def test1():
    
    do = DoHTTP('localhost', '4569', verbose=True)
    
    do.open_repository('test2')
    
    do.get_repositories()
    
    do.get_size()
    
    do.get_namespaces()
        
    do.get_all_statements()
    
    do.get_statements('', '', '"Smith"')
    
    do.get_statements('person1', 'firstname', 'steve')
    
if __name__ == '__main__':
    choices = [i for i in range(1,17)]
    choices = [1]
    for choice in choices:
        print "\n==========================================================================="
        print "Test Run Number ", choice, "\n"
        if choice == 1: test1()
#        elif choice == 2: test2()
#        elif choice == 3: test3()
#        elif choice == 4: test4()    
#        elif choice == 5: test5()        
#        elif choice == 6: test6()            
#        elif choice == 7: test7()                
#        elif choice == 8: test8()                
#        elif choice == 9: test9()                        
#        elif choice == 10: test10()                            
