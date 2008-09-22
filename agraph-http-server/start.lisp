(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))

(in-package :agraph-http-server)

(defvar *data* (namestring (merge-pathnames "data" (asdf:component-pathname (asdf:find-component nil :agraph-http-server)))))

(setf *server* (make-instance 'agraph-http-server :cache-file *data*))

(start :port 8080)
(setf net.aserve::*enable-logging* nil)
(publish-http-server *wserver* *server*)

(cond ((find-store-spec "foo")
       (format *query-io* "~%~%Enter the path of a triple-store to publish: ")
       (let ((file (read-line *query-io*)))
         (open-store *server* "foo" file nil)
         (format *query-io* "~% !! ~a published under id 'foo'.~%~%" file)))
      (t (format *query-io* "~%~% !! Store 'foo' already set.~%~%")))

