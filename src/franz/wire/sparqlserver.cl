

(defpackage sparql-server-example  
  (:use :cl :excl :db.agraph  
        :db.agraph.sparql  
        :sparql.server))  
 
(in-package :sparql-server-example)  

(create-triple-store "/tmp/protocol-example") 
(load-ntriples "sys:agraph;tutorial-files;wilburwine.ntriples")  
(index-all-triples)  
(make-sparql-protocol-server :start '(:port 7654))

;; example
;; (net.aserve.client:do-http-request "http://localhost:7654/sparql"  
;;        :query '((query . "SELECT ?s ?o { ?s a ?o . } LIMIT 5")))  