(in-package :agraph-http-tests)

(let ((base (asdf:component-pathname (asdf:find-component nil :agraph-http-tests))))
  (defun package-path (dir &optional as-string)
    (let ((path (merge-pathnames dir base)))
      (if as-string (namestring path) path))))

(defmacro with-server ((&rest args) &body body)
  (let ((server (gensym))
        (data (gensym)))
    `(let ((,data (package-path "test/server/" t))
           (,server nil))
       (setf ,server (make-instance 'agraph-http-server :cache-file ,data ,@args
                                    :directory (package-path "test/repositories/")))
       (start :port 8080)
       (publish-http-server net.aserve:*wserver* ,server)
       (unwind-protect (progn ,@body)
         (close-http-server ,server)
         (net.aserve:shutdown)
         (delete-directory-and-files ,data)
         (delete-directory-and-files (package-path "test/repositories/"))))))

(defmacro with-client ((name &rest args) &body body)
  `(let ((,name (make-instance 'agraph-client :url "http://localhost:8080" ,@args)))
     ,@body))

(defmacro test-if (check &rest args)
  `(test t (and ,check t) ,@args))