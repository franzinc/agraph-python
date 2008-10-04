(eval-when (compile eval load)
  (require :tester))

(defpackage :agraph-http-tests
  (:shadowing-import-from :agraph-http-client #:catalog) 
  (:use :cl :agraph-http-server :agraph-http-client :util.test :excl :st-json))
