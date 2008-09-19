;; TODO persistence

(in-package :agraph-http-server)

(defclass simple-server ()
  ((stores :initform nil :accessor @stores)))

(defun server-add-store (server id &key file db)
  (assert (or file db))
  (unless db (setf db (open-triple-store file)))
  (push (cons id db) (@stores server)))

(defun find-store (server id)
  (cdr (find id (@stores server) :test #'string= :key #'car)))
