(eval-when (compile eval load)
  (require :aserve))

(defpackage :agraph-http-client
  (:use :cl :net.aserve :net.aserve.client :st-json :excl)
  (:export #:agraph-client
           #:list-triple-stores #:get-repository
           #:open-triple-store #:create-triple-store #:close-triple-store
           #:repository #:repository-environment
           #:repository-size #:repository-contexts #:repository-writeable-p
           #:sparql-query #:prolog-query #:define-prolog-functor
           #:get-statements #:add-statement #:delete-matching-statements
           #:add-statements #:delete-statements #:add-ntriples-statements
           #:repository-indices #:add-index #:delete-index
           #:repository-index-coverage #:index-repository
           #:freetext-search #:repository-freetext-predicates #:freetext-register-predicate
           #:repository-environments #:create-environment #:delete-environment
           #:repository-namespaces #:define-namespace #:delete-namespace))
