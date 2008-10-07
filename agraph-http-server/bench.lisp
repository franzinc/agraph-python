(require :asdf)
(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))
(asdf:oos 'asdf:load-op :agraph-http-client)

(setf net.aserve::*enable-logging* nil)

;; Set up a server with a small store
(in-package :agraph-http-server)
(defvar *path* (make-pathname :name "tempstore" :type nil :defaults *load-pathname*))
(defvar *s* (create-server :port 8080))
(add-catalog *s* *path*)

(in-package :agraph-http-client)
(defvar *c* (make-instance 'agraph-http-client :url "http://localhost:8080"))
(defvar *cat* (get-catalog *c* "/catalogs/tempstore"))
(create-triple-store *cat* "foo")
(defvar *repo* (get-repository *cat* "foo"))
(add-statements *repo* (loop :for i :below 50
                             :collect (list (format nil "<http://foo.com#~a>" i) "<http://bar.com>"
                                            (format nil "\"~a\"" i) :null)))

(format t "~%Timing 1000 1-row queries.")
(time (dotimes (i 1000)
        (sparql-query *repo* "select ?x ?y ?z {?x ?y ?z} limit 1")))

(format t "~%Timing 500 10-row queries.")
(time (dotimes (i 500)
        (sparql-query *repo* "select ?x ?y ?z {?x ?y ?z} limit 10")))

;; Clean up
(in-package :agraph-http-server)
(shutdown-server *s*)
(delete-directory-and-files *path*)
