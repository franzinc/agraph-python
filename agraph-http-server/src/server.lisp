;; TODO user accounts

(in-package :agraph-http-server)

(defclass agraph-http-server ()
  ((cache :reader @cache)
   (lock :initform (mp:make-process-lock) :reader @lock)
   (open-stores :initform (make-hash-table :test 'equal) :reader @open-stores)))

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
   (read-only :initarg :read-only :reader @read-only))
  (:metaclass persistent-class))

(defun find-store-spec (name)
  (retrieve-from-index 'store-spec 'name name))

(defun store-from-spec (spec)
  (open-triple-store (@file spec) :read-only (@read-only spec)))

(defun get-store (server name)
  (or (gethash name (@open-stores server))
      (with-server-cache (server)
        (let ((spec (find-store-spec name)))
          (and spec (set-store server name (store-from-spec spec)))))))

(defun set-open-store (server name db)
  (setf (gethash name (@open-stores server)) db))

(defun del-open-store (server name)
  (remhash name (@open-stores server)))

(defun open-store (server name file read-only)
  (with-server-cache (server t)
    (request-assert (not (find-store-spec name)) "A repository named '~a' already exists." name)
    (let ((spec (make-instance 'store-spec :name name :file file :read-only read-only)))
      (set-open-store *server* name (store-from-spec spec)))))

(defun create-store (server name file)
  (let (*db*)
    (handler-case (create-triple-store file)
      (error (e) (request-failed *response-internal-server-error* (princ-to-string e))))
    (close-triple-store))
  (open-store server name file nil))

(defun close-store (server name)
  (with-server-cache (server t)
    (let ((spec (find-store-spec name)))
      (request-assert spec "No repository named '~a' known." name)
      (del-open-store server name)
      (delete-instance spec))))
