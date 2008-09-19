;; TODO opening and closing stores

(in-package :agraph-http-server)

(defservice (:get :nostore) "repositories" ()
  (values :list (mapcar #'car (@stores *server*))))

(defun reasoning (on)
  (when on (setf *db* (db-apply-reasoner *db* 'rdfs++-reasoner (db-name *db*)))))

(defun assert-part (string)
  (or (rep-part string) (request-failed "~a is not a valid part." string)))

(defun assert-context (string)
  (if (string= string "null") (default-graph-upi *db*) (assert-part string)))

(defservice (:get :post) "" ((query :string) (infer :boolean nil) (context :list) ((lang "queryLn") :string "sparql"))
  (reasoning infer)
  (cond ((string-equal lang "sparql")
         (sparql-query query context))
        ((string-equal lang "prolog")
         (request-assert (not context) "Contexts not supported for Prolog queries.")
         (prolog-query query))
        (t (request-failed "Unsupported query language: '~a'" lang))))

(defun sparql-query (query context)
  (multiple-value-bind (parsed error) (ignore-errors (parse-sparql query))
    (when error (request-failed (princ-to-string error)))
    (when (member "null" context :test #'string=)
      (if (= (length context) 1)
          (setf context nil)
          (request-failed "Can not use null context along with other contexts in a SPARQL query.")))
    (setf context (mapcar #'assert-part context))
    (multiple-value-bind (result verb names)
        (handler-case (run-sparql parsed :engine :algebra :rdf-format :cursor
                                  :results-format (if (eq (second parsed) :ask) :boolean :cursor)
                                  :from-named context)
          (error (e) (request-failed (princ-to-string e))))
      (ecase verb
        (:ask (values :boolean result))
        (:select (values :row-cursor (wrap-algebra-row-cursor result (mapcar #'symbol-name names))))
        (:construct (values :triple-cursor (wrap-algebra-triple-cursor result)))
        (:describe (values :triple-cursor result))))))

(defun prolog-query (query)
  (handler-case (multiple-value-bind (values names) (run-prolog query)
                  (values :row-cursor (wrap-list-cursor values names)))
    (error (e) (request-failed (princ-to-string e)))))

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

(defservice :post "statements" ((subj :string) (pred :string) (obj :string) (context :string nil))
  (add-triple (assert-part subj) (assert-part pred) (assert-part obj) :g (and context (assert-part context)))
  :null)

(defservice :delete "statements" ((subj :string) (pred :string) (obj :string) (context :string nil))
  (let ((triple (get-triple :s (assert-part subj) :p (assert-part pred)
                            :o (assert-part obj) :g (and context (assert-part context)))))
    (when triple (delete-triple (triple-id triple))))
  :null)

(defservice :get "size" ()
  (values :integer (triple-count)))

(defservice :get "indices" ()
  (values :list (mapcar (lambda (s) (string-downcase (symbol-name s))) (triple-store-indices))))

(defun assert-valid-index (type)
  (request-assert (member type '("spogi" "posgi" "ospgi" "gspoi" "gposi" "gospi") :test #'string=)
                  "'~a' is not a valid index type." type))

(defservice :post "indices" ((type :string))
  (assert-valid-index type)
  (add-index (intern-with-default-case type :keyword))
  :null)

(defservice :delete "indices" ((type :string))
  (assert-valid-index type)
  (drop-index (intern-with-default-case type :keyword))
  :null)

(defservice :get "index" ()
  (values :float (index-coverage-percent)))

(defservice :post "index" ((all :boolean nil))
  (if all (index-all-triples) (index-new-triples))
  :null)

(defservice :get "freetextindices" ()
  (values :list (mapcar (lambda (n) (format nil "<~a>" n))
                        (freetext-registered-predicates))))

(defservice :post "freetextindices" ((predicate :string))
  (let ((part (assert-part predicate)))
    (request-assert (char= (char predicate 0) #\<) "Text indexing predicates must be URIs.")
    (register-freetext-predicate part))
  :null)

(defservice :post "statements/ntriples" ((body :postbody))
  (handler-case (load-ntriples-from-string body)
    (error (e) (request-failed (princ-to-string e))))
  :null)

(defservice :post "statements/rdfxml" ((body :postbody))
  (handler-case (load-rdf/xml-from-string body)
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
  (loop :for (subj pred obj graph) :in (canonise-json-statements body)
        :do (add-triple subj pred obj :g (or graph (default-graph-upi *db*))))
  :null)

(defservice :post "statements/json/delete" ((body :postbody))
  (loop :for (subj pred obj graph) :in (canonise-json-statements body)
        :do (let ((triple (get-triple :s subj :p pred :o obj :g (or graph (default-graph-upi *db*)))))
              (when triple (delete-triple (triple-id triple)))))
  :null)
