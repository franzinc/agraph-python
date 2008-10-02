(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))

(in-package :agraph-http-server)

(defvar *data* (namestring (merge-pathnames "data" (asdf:component-pathname (asdf:find-component nil :agraph-http-server)))))

(start :port 8080)
(setf net.aserve::*enable-logging* nil)

(cond ((find-store-spec "foo")
       (format *query-io* "~%~%Enter the directory in which your triple-stores live: ")
       (let ((dir (read-line *query-io*)))
         (setf *server* (make-instance 'agraph-http-server :cache-file *data* :directory dir))
         (publish-http-server *wserver* *server*)))
