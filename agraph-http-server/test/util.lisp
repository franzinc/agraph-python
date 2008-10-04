(in-package :agraph-http-tests)

(let ((base (asdf:component-pathname (asdf:find-component nil :agraph-http-tests))))
  (defun package-path (dir &optional as-string)
    (let ((path (merge-pathnames dir base)))
      (if as-string (namestring path) path))))

(defmacro with-server (&body body)
  (let ((server (gensym))
        (dir (gensym)))
    `(let ((,dir (package-path "test/catalog/" t))
           (,server (create-server :port 8080)))
       (add-catalog ,server ,dir)
       (unwind-protect (progn ,@body)
         (shutdown-server ,server)
         (delete-directory-and-files ,dir)))))

(defmacro test-if (check &rest args)
  `(test t (and ,check t) ,@args))