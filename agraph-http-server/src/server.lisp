;; TODO user accounts

(in-package :agraph-http-server)

(defclass agraph-http-server ()
  ((cache :reader @cache)
   (lock :initform (mp:make-process-lock) :reader @lock)
   (open-stores :initform (make-hash-table :test 'equal) :reader @open-stores)
   (open-environments :initform (make-hash-table :test 'equal) :reader @open-environments)
   (username :initform nil :initarg :username :reader @username)
   (password :initform nil :initarg :password :reader @password)))

(defmacro with-server-cache ((server &optional write-p) &body body)
  `(flet ((body () ,@body))
     (let* ((server ,server)
            (*allegrocache* (@cache server)))
       (mp:with-process-lock ((@lock server))
         (rollback)
         ,(if write-p '(prog1 (body) (commit)) '(body))))))

(defmethod initialize-instance :after ((server agraph-http-server) &key cache-file &allow-other-keys)
  (assert cache-file)
  (setf (slot-value server 'cache)
        (let (*allegrocache*) (open-file-database cache-file :if-does-not-exist :create))))

(defclass store-spec ()
  ((name :initarg :name :reader @name :index :any-unique)
   (file :initarg :file :reader @file)
   (read-only :initarg :read-only :reader @read-only)
   (anon-access :initarg :anon-access :accessor @anon-access))
  (:metaclass persistent-class))

(defun find-store-spec (name)
  (retrieve-from-index 'store-spec 'name name))

(defclass open-store ()
  ((db :initarg :db :reader @db)
   (spec :initarg :spec :reader @spec)
   (reasoning-db :initarg :reasoning-db :reader @reasoning-db)))

(defun make-store (db spec)
  (make-instance 'open-store :db db :spec spec
                 :reasoning-db (db-apply-reasoner *db* 'rdfs++-reasoner (db-name *db*))))

(defun store-from-spec (spec)
  (handler-case (make-store (open-triple-store (@file spec) :read-only (@read-only spec)) spec)
    (error (e) (request-failed* *response-internal-server-error* (princ-to-string e)))))

(defun get-store (server name)
  (or (gethash name (@open-stores server))
      (with-server-cache (server)
        (let ((spec (find-store-spec name)))
          (and spec (setf (gethash name (@open-stores server))
                          (store-from-spec spec)))))))

(defun open-store (server name file read-only)
  (with-server-cache (server t)
    (request-assert (not (find-store-spec name)) "A repository named '~a' already exists." name)
    (let ((spec (make-instance 'store-spec :name name :file file :read-only read-only)))
      (setf (gethash name (@open-stores server)) (store-from-spec spec)))))

(defun create-store (server name file)
  (let (*db*)
    (handler-case (create-triple-store file)
      (error (e) (request-failed* *response-internal-server-error* (princ-to-string e))))
    (close-triple-store))
  (open-store server name file nil))

(defun close-store (server name)
  (with-server-cache (server t)
    (let ((spec (find-store-spec name)))
      (request-assert spec "No repository named '~a' known." name)
      (remhash name (@open-stores server))
      (delete-instance spec))))

(defparameter *default-namespaces*
  '(("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
    ("owl" "http://www.w3.org/2002/07/owl#")
    ("dc" "http://purl.org/dc/elements/1.1/")
    ("dcterms" "http://purl.org/dc/terms/")
    ("foaf" "http://xmlns.com/foaf/0.1/")
    ("fti" "http://franz.com/ns/allegrograph/2.2/textindex/")))

(defclass environment ()
  ((id :initarg :id :reader @id :index :any-unique)
   (namespaces :initform *default-namespaces* :accessor @namespaces))
  (:metaclass persistent-class))

(defun get-environment (server store name)
  (or (with-server-cache (server)
        (retrieve-from-index 'environment 'id (if name (list store name) store)))
      (and (not name)
           (with-server-cache (server t)
             (make-instance 'environment :id store)))))

(defun list-environments (server store)
  (let ((names ()))
    (with-server-cache (server)
      (doclass (env 'environment)
        (when (and (consp (@id env)) (string= (first (@id env)) store))
          (push (second (@id env)) names))))
    (nreverse names)))
