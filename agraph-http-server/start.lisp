(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))

(in-package :agraph-http-server)

(start :port 8080)
(setf net.aserve::*enable-logging* nil)

(format *query-io* "~%~%Enter the directory in which your triple-stores live: ")
(setf *server* (make-instance 'agraph-http-server :directory (read-line *query-io*)))
(publish-http-server *wserver* *server*)
