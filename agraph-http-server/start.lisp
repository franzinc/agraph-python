(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))

(in-package :agraph-http-server)

(setf *server* (make-instance 'simple-server))
(start :port 8080)
(setf net.aserve::*enable-logging* nil)
(publish-simple-server *wserver* *server*)

(format *query-io* "~%~%Enter the path of a triple-store to publish: ")
(let ((file (read-line *query-io*)))
  (server-add-store *server* "foo" :file file)
  (format *query-io* "~% !! ~a published under id 'foo'.~%~%" file))
