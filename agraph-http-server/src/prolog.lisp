(in-package :agraph-http-server)

;; A package to intern the symbols from prolog queries into.
(defvar *prolog-query-package* (make-package (gensym "prolog-query-package") :use '(:db.agraph :prolog)))
;; Any symbols not from these packages may not occur in a query.
(defparameter *allowed-packages* (cons *prolog-query-package* (mapcar #'find-package '(:db.agraph :prolog :cl))))
;; A blacklist of functors that run arbitrary Lisp code.
(defparameter *forbidden-symbols* '(?? lispp* lisp*! lisp* lisp lispp! lispp! *lisp! lispp))
;; A readtable with AGraph !-syntax enabled.
(eval-when (compile eval load)
  (let ((*readtable* (copy-readtable)))
    (enable-!-reader)
    (defparameter *!-read-table* *readtable*)))

(defun walk-prolog (form)
  "Go over a parsed Prolog query checking for disallowed symbols."
  (cond ((consp form) (and (walk-prolog (car form)) (walk-prolog (cdr form))))
        ((symbolp form) (and (not (member form *forbidden-symbols*))
                             (member (symbol-package form) *allowed-packages*)))
        (t t)))

(defun validate-prolog-query (expr)
  "Verify a Prolog query. Use 'select0' for efficiency, make sure a
list of results is selected, and check for forbidden symbols."
  (unless (listp expr) (error "Prolog queries should be lists."))
  (destructuring-bind (method args &rest forms) expr
    `(,(case method
         ((select select0) 'select0)
         ((select-distinct select-distinct0) 'select-distinct0)
         (t (error "Invalid prolog query type: ~a" method)))
      ,(cond ((symbolp args) (list args))
             ((and (proper-list-p args) (every #'symbolp args)) args)
             (t (error "Only regular lists of bindings are supported.")))
      ,@(if (walk-prolog forms)
            forms
            (error "Illegal elements in query.")))))

(defun run-prolog (query)
  "Read, validate, and run a Prolog query."
  (let* ((*read-eval* nil)
         (*package* *prolog-query-package*)
         (*readtable* *!-read-table*)
         (expr (validate-prolog-query (read-from-string query))))
    (values (funcall (compile nil (list 'lambda nil expr)))
            (mapcar #'symbol-name (second expr)))))
