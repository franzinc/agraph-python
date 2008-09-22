(in-package :agraph-http-server)

(defstruct service store-p args result func methods)

(defparameter *services* (make-hash-table :test 'equal))

(defun service (methods name args func store-p)
  (flet ((string-name (spec)
           (let ((name (car spec)))
             (cons (etypecase name (symbol (string-downcase (symbol-name name))) (cons (second name)))
                   (cdr spec)))))
    (let ((s (make-service :store-p store-p
                           :args (mapcar #'string-name args)
                           :func func)))
      (dolist (method methods)
        (setf (gethash (cons name method) *services*) s)))))

(defmacro defservice (options name (&rest args) &body body)
  (unless (consp options) (setf options (list options)))
  `(service ',(intersection options '(:get :post :put :delete)) ,name ',args
            (lambda ,(mapcar (lambda (n) (etypecase (car n) (symbol (car n)) (cons (caar n)))) args)
              ,@body)
            ,(not (member :nostore options))))

(defun find-service (path method)
  (gethash (cons path method) *services*))

(defvar *server*)
(defvar *store*)

(defun call-service (service parameters)
  (apply (service-func service) parameters))

(defun interpret-parameters (service reader)
  (mapcar (lambda (p) (apply reader p)) (service-args service)))

(defgeneric read-type (type string)
  (:method ((type (eql :string)) string)
    (values string t))
  (:method ((type (eql :boolean)) string)
    (cond ((string= string "true") (values t t))
          ((string= string "false") (values nil t))
          (t (values nil nil))))
  (:method ((type (eql :integer)) string)
    (handler-case (values (parse-integer string) t)
      (error () (values nil nil)))))
