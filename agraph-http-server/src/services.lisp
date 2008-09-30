;; TODO opening and closing stores

(in-package :agraph-http-server)

(defservice (:get :nostore) "repositories" ()
  (let ((names ()))
    (with-server-cache (*server*)
      (doclass (spec 'store-spec)
        (push (@name spec) names)))
    (values :list names)))

(defservice (:post :nostore) "repository/open" ((name :string) (file :string) ((read-only "readOnly") :boolean nil))
  (open-store *server* name file read-only)
  :null)

(defservice (:post :nostore) "repository/create" ((name :string) (file :string))
  (create-store *server* name file)
  :null)

(defservice :delete "" ()
  (close-store *server* *store*)
  :null)


(defun reasoning (on)
  (when on (setf *db* (@reasoning-db *store*))))

(defun assert-part (string)
  (or (rep-part string) (request-failed "~a is not a valid part." string)))

(defun assert-context (string)
  (if (string= string "null") (default-graph-upi *db*) (assert-part string)))

(defun store-name ()
  (@name (@spec *store*)))

(defun assert-environment (name)
  (or (get-environment *server* (store-name) name)
      (request-failed "Environment '~a' not found." name)))

(defservice (:get :post) "" ((query :string) (infer :boolean nil) (context :list)
                             ((lang "queryLn") :string "sparql")
                             (environment :string nil))
  (reasoning infer)
  (let ((env (assert-environment environment)))
    (cond ((string-equal lang "sparql")
           (sparql-query query context (@namespaces env)))
          ((string-equal lang "prolog")
           (request-assert (not context) "Contexts not supported for Prolog queries.")
           (prolog-query query env))
          (t (request-failed "Unsupported query language: '~a'" lang)))))

(defun sparql-query (query context namespaces)
  (multiple-value-bind (parsed error) (ignore-errors (parse-sparql query namespaces))
    (when error (request-failed (princ-to-string error)))
    (when (member "null" context :test #'string=)
      (if (= (length context) 1)
          (setf context nil)
          (request-failed "Can not use null context along with other contexts in a SPARQL query.")))
    (setf context (mapcar #'assert-part context))
    (multiple-value-bind (result verb names)
        (handler-case (run-sparql parsed :engine :algebra :rdf-format :cursor
                                  :results-format (if (eq (second parsed) :ask) :boolean :cursor)
                                  :from context)
          (error (e) (request-failed (princ-to-string e))))
      (ecase verb
        (:ask (values :boolean result))
        (:select (values :row-cursor (wrap-algebra-row-cursor result (mapcar #'symbol-name names))))
        (:construct (values :triple-cursor (wrap-algebra-triple-cursor result)))
        (:describe (values :triple-cursor result))))))

(defun prolog-query (query env)
  (handler-case (multiple-value-bind (values names) (run-prolog query (@namespaces env) (@prolog-package env))
                  (values :row-cursor (wrap-list-cursor values names)))
    (error (e) (request-failed (princ-to-string e)))))

(defservice :post "functor" ((definition :string) (environment :string nil))
  (let ((env (assert-environment environment)))
    (handler-case (eval-prolog definition (@namespaces env) (@prolog-package env))
      (error (e) (request-failed (princ-to-string e))))
    (with-server-cache (*server* t)
      (push definition (@functors env))))
  :null)

(defservice (:get :post) "freetext" ((pattern :string) (infer :boolean nil))
  (reasoning infer)
  (values :triple-cursor (freetext-get-triples pattern)))

(defservice :get "contexts" ()
  (values :list (mapcar 'part-rep (remove (default-graph-upi *db*) (find-unique-upis-for-column :graph)))))
  
(defservice :get "statements" ((subj :string nil) (pred :string nil) (obj :string nil)
                               (context :list) (infer :boolean nil))
  (reasoning infer)
  (flet ((part (val) (and val (assert-part val))))
    (setf subj (part subj) pred (part pred) obj (part obj)))
  (flet ((triples (graph) (get-triples :s subj :p pred :o obj :g graph)))
    (values :triple-cursor
            (if (null context)
                (triples nil)
                (append-cursors (mapcar (lambda (cx) (triples (assert-context cx))) context))))))

(defun store-writeable ()
  (not (db.agraph::read-only-p *db*)))

(defun assert-writeable ()
  (request-assert (store-writeable) "Attempting write operation on a read-only repository."))

(defservice :post "statements" ((subj :string) (pred :string) (obj :string) (context :string nil))
  (assert-writeable)
  (add-triple (assert-part subj) (assert-part pred) (assert-part obj) :g (and context (assert-part context)))
  :null)

(defservice :delete "statements" ((subj :string nil) (pred :string nil) (obj :string nil) (context :string nil))
  (assert-writeable)
  (flet ((part (val) (and val (assert-part val))))
    (delete-triples :s (part subj) :p (part pred) :o (part obj) :g (part context)))
  :null)

(defservice :get "size" ()
  (values :integer (triple-count)))

(defservice :get "writeable" ()
  (values :boolean (store-writeable)))

(defservice :get "indices" ()
  (values :list (mapcar (lambda (s) (string-downcase (symbol-name s))) (triple-store-indices))))

(defun assert-valid-index (type)
  (request-assert (member type '("spogi" "posgi" "ospgi" "gspoi" "gposi" "gospi") :test #'string=)
                  "'~a' is not a valid index type." type))

(defservice :post "indices" ((type :string))
  (assert-writeable)
  (assert-valid-index type)
  (add-index (intern-with-default-case type :keyword))
  :null)

(defservice :delete "indices" ((type :string))
  (assert-writeable)
  (assert-valid-index type)
  (drop-index (intern-with-default-case type :keyword))
  :null)

(defservice :get "index" ()
  (values :float (index-coverage-percent)))

(defservice :post "index" ((all :boolean nil))
  (assert-writeable)
  (if all (index-all-triples) (index-new-triples))
  :null)

(defservice :get "freetextpredicates" ()
  (values :list (mapcar (lambda (n) (format nil "<~a>" n))
                        (freetext-registered-predicates))))

(defservice :post "freetextpredicates" ((predicate :string))
  (assert-writeable)
  (let ((part (assert-part predicate)))
    (request-assert (char= (char predicate 0) #\<) "Text indexing predicates must be URIs.")
    (register-freetext-predicate part))
  :null)

(defservice :post "statements/ntriples" ((body :postbody))
  (assert-writeable)
  (handler-case (load-ntriples-from-string body)
    (error (e) (request-failed (princ-to-string e))))
  :null)

(defservice :post "statements/load/ntriples" ((file :string))
  (handler-case (load-ntriples file)
    (error (e) (request-failed (princ-to-string e))))
  :null)
    
(defservice :post "statements/rdfxml" ((body :postbody))
  (assert-writeable)
  (handler-case (load-rdf/xml-from-string body)
    (error (e) (request-failed (princ-to-string e))))
  :null)

(defservice :post "statements/load/rdfxml" ((file :string))
  (handler-case (load-rdf/xml file)
    (error (e) (request-failed (princ-to-string e))))
  :null)

(defun canonise-json-statements (input)
  (let ((rows (handler-case (read-json-as-type input '(list-of (list-of (or string json-null) 4)))
                (error (e) (request-failed (princ-to-string e))))))
    (dolist (row rows)
      (map-into row (lambda (str) (if (eq str :null) nil (assert-part str))) row)
      (destructuring-bind (subj pred obj graph) row
        (request-assert (and subj pred obj) "The subject, predicate, and object of a statement must not be null.")
        (request-assert (or (not graph) (eq (nth-value 1 (part-value graph)) :node))
                        "Graph names must be either null or a URI.")))
    rows))

(defservice :post "statements/json" ((body :postbody))
  (assert-writeable)
  (loop :for (subj pred obj graph) :in (canonise-json-statements body)
        :do (add-triple subj pred obj :g (or graph (default-graph-upi *db*))))
  :null)

(defservice :post "statements/json/delete" ((body :postbody))
  (assert-writeable)
  (loop :for (subj pred obj graph) :in (canonise-json-statements body)
        :do (let ((triple (get-triple :s subj :p pred :o obj :g (or graph (default-graph-upi *db*)))))
              (when triple (delete-triple (triple-id triple)))))
  :null)

;; Managing environments.

(defservice :get "namespaces" ((environment :string nil))
  (values :namespaces (@namespaces (assert-environment environment))))

(defservice :post "namespaces" ((prefix :string) (uri :string) (environment :string nil))
  (let ((env (assert-environment environment)))
    (with-server-cache (*server* t)
      (setf (@namespaces env)
            (cons (list prefix uri) (remove prefix (@namespaces env) :key #'car :test #'string=)))))
  :null)

(defservice :delete "namespaces" ((prefix :string) (environment :string nil))
  (let ((env (assert-environment environment)))
    (with-server-cache (*server* t)
      (setf (@namespaces env) (remove prefix (@namespaces env) :key #'car :test #'string=))))
  :null)

(defservice :get "environments" ()
  (values :list (list-environments *server* (store-name))))

(defservice :post "environments" ((name :string))
  (request-assert (not (get-environment *server* (store-name) name))
                  "An environment named '~a' already exists.")
  (with-server-cache (*server* t)
    (make-instance 'environment :id (list (store-name) name)))
  :null)

(defservice :delete "environments" ((name :string))
  (let ((env (assert-environment name)))
    (with-server-cache (*server* t)
      (delete-instance env)))
  :null)
