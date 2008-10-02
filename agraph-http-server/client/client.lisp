(in-package :agraph-http-client)

(defclass agraph-client (http-service) ())

(defun repo-url (name)
  (format nil "/repositories/~a" (urlenc name)))

(defun list-triple-stores (clnt)
  (json-request clnt :get "/repositories"))

(defun create-triple-store (clnt name)
  (null-request clnt :put "/repositories/~a" (urlenc name)))

(defun delete-triple-store (clnt name)
  (null-request clnt :delete (repo-url name)))

(defun get-repository (clnt name &optional environment)
  (make-instance 'repository :url (concatenate 'string (@url-prefix clnt) (repo-url name))
                 :auth (@auth clnt) :environment environment))


(defclass repository (http-service)
  ((environment :initform nil :initarg :environment :accessor repository-environment)))

(defun repository-size (rep)
  (json-request rep :get "/size"))

(defun repository-contexts (rep)
  (json-request rep :get "/contexts"))

(defun repository-writeable-p (rep)
  (json-request rep :get "/writeable"))

(defun sparql-query (rep query &optional infer contexts)
  (json-request rep :post "" (urlenc "query" query "infer" (boolstr infer) "context" contexts
                                     "environment" (repository-environment rep))))

(defun prolog-query (rep query &optional infer)
  (json-request rep :post "" (urlenc "query" query "infer" (boolstr infer) "queryLn" "prolog"
                                     "environment" (repository-environment rep))))

(defun define-prolog-functor (rep definition)
  (null-request rep :post "/functor" (urlenc "definition" definition "environment" (repository-environment rep))))

(defun get-statements (rep &key subj pred obj contexts infer)
  (json-request rep :get "/statements" (urlenc "subj" subj "pred" pred "obj" obj "context" contexts "infer" (boolstr infer))))

(defun add-statement (rep subj pred obj &optional context)
  (null-request rep :post "/statements" (urlenc "subj" subj "pred" pred "obj" obj "context" context)))

(defun delete-matching-statements (rep &key subj pred obj context)
  (null-request rep :delete "/statements" (urlenc "subj" subj "pred" pred "obj" obj "context" context)))

(defun add-statements (rep quads)
  (null-request rep :post "/statements/json" (write-json-to-string quads) "application/json"))

(defun add-ntriples-statements (rep &key file string)
  (unless string
    (assert file)
    (setf string (read-text-file file)))
  (null-request rep :post "/statements/ntriples" string "text/plain"))

(defun delete-statements (rep quads)
  (null-request rep :post "/statements/json/delete" (write-json-to-string quads) "application/json"))

(defun repository-indices (rep)
  (json-request rep :get "/indices"))

(defun add-index (rep type)
  (null-request rep :post "/indices" (urlenc "type" type)))

(defun delete-index (rep type)
  (null-request rep :delete "/indices" (urlenc "type" type)))

(defun repository-index-coverage (rep)
  (json-request rep :get "/index"))

(defun index-repository (rep &optional all)
  (null-request rep :post "/index" (urlenc "all" (boolstr all))))

(defun freetext-search (rep pattern &optional infer)
  (json-request rep :get "/freetext" (urlenc "pattern" pattern "infer" (boolstr infer))))

(defun repository-freetext-predicates (rep)
  (json-request rep :get "/freetextpredicates"))

(defun freetext-register-predicate (rep predicate)
  (null-request rep :post "/freetextpredicates" (urlenc "predicate" predicate)))

(defun repository-environments (rep)
  (json-request rep :get "/environments"))

(defun create-environment (rep name)
  (null-request rep :post "/environments" (urlenc "name" name)))

(defun delete-environment (rep name)
  (null-request rep :delete "/environments" (urlenc "name" name)))

(defun repository-namespaces (rep)
  (json-request rep :get "/namespaces" (urlenc "environment" (repository-environment rep))))

(defun define-namespace (rep prefix uri)
  (null-request rep :post "/namespaces" (urlenc "prefix" prefix "uri" uri
                                                "environment" (repository-environment rep))))

(defun delete-namespace (rep prefix uri)
  (null-request rep :delete "/namespaces" (urlenc "prefix" prefix "uri" uri
                                                  "environment" (repository-environment rep))))
