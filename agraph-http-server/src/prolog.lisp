(in-package :agraph-http-server)

;; A package to intern the symbols from prolog queries into.
(defvar *prolog-query-package* (make-package (gensym "prolog-query-package") :use '(:db.agraph :prolog)))
;; Any symbols not from these packages may not occur in a query.
(defparameter *allowed-packages* (mapcar #'find-package '(:db.agraph :prolog :cl)))
;; A blacklist of functors that run arbitrary Lisp code.
(defparameter *forbidden-symbols* '(?? lispp* lisp*! lisp* lisp lispp! lispp! *lisp! lispp))
;; A readtable with AGraph !-syntax enabled.
(eval-when (compile eval load)
  (let ((*readtable* (copy-readtable)))
    (enable-!-reader)
    (defparameter *!-read-table* *readtable*)))

(defmacro with-namespaces (namespaces &body body)
  `(let ((db.agraph::*namespaces* (make-hash-table :test 'equal)))
     (loop :for (prefix uri) :in ,namespaces
           :do (register-namespace prefix uri))
     ,@body))

(defun read-prolog-expr (string package)
  (let ((*read-eval* nil)
        (*package* package)
        (*readtable* *!-read-table*))
    (read-from-string string)))

(defun walk-prolog (form package)
  "Go over a parsed Prolog query checking for disallowed symbols."
  (let ((packages (cons package *allowed-packages*)))
    (labels ((walk (form)
               (cond ((consp form) (and (walk (car form)) (walk (cdr form))))
                     ((symbolp form) (and (not (member form *forbidden-symbols*))
                                          (member (symbol-package form) packages)))
                     (t t))))
      (walk form))))

(defun validate-prolog-query (expr package)
  "Verify a Prolog query. Use 'select0' for efficiency, make sure a
list of results is selected, and check for forbidden symbols."
  (unless (listp expr) (error "Prolog queries should be lists."))
  (destructuring-bind (method args &rest forms) expr
    `(,(case method
         ((select select0) 'select0)
         ((select-distinct select-distinct0) 'select-distinct0)
         (t (error "Invalid Prolog query type: ~a" method)))
      ,(cond ((symbolp args) (list args))
             ((and (proper-list-p args) (every #'symbolp args)) args)
             (t (error "Only regular lists of bindings are supported.")))
      ,@(if (walk-prolog forms package)
            forms
            (error "Illegal elements in Prolog query.")))))

(defun run-prolog (query namespaces package)
  "Read, validate, and run a Prolog query."
  (with-namespaces namespaces
    (let ((expr (read-prolog-expr query package)))
      (setf expr (validate-prolog-query expr package))
      (values (funcall (compile nil (list 'lambda nil expr)))
              (mapcar #'symbol-name (second expr))))))

(defun validate-prolog-definition (expr package)
  (unless (listp expr) (error "Prolog definitions should be lists."))
  (unless (member (car expr) '(prolog:<-- prolog:<-))
    (error "Prolog definitions must start with <-- or <-."))
  (unless (walk-prolog (cdr expr) package)
    (error "Illegal elements in Prolog definition.")))

(defun eval-prolog (definition namespaces package)
  (with-namespaces namespaces
    (let ((expr (read-prolog-expr definition package)))
      (validate-prolog-definition expr package)
      (funcall (compile nil (list 'lambda nil expr))))))

(defun make-prolog-package (functor-defs namespaces)
  (let ((p (make-package (gensym "prolog-package") :use '(:db.agraph :prolog))))
    (loop :for def :in (reverse functor-defs)
          :do (eval-prolog def namespaces p))
    p))
