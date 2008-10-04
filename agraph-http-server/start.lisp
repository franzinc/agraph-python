(unless (find-package :agraph-http-server)
  (load (merge-pathnames #p"load.lisp" *load-pathname*)))

(in-package :agraph-http-server)

(setf net.aserve::*enable-logging* nil)

(format *query-io* "~%~%Enter a port number (leave blank for 8080): ")
(let ((port (or (ignore-errors (parse-integer (read-line *query-io*))) 8080)))
  (defvar *server* (create-server :port port))
  (format *query-io* "Running at port ~a." port))

(princ #\newline *query-io*)
(loop
 (format *query-io* "~%Enter a directory with triple stores to publish (leave blank to go to REPL): ")
 (let ((dir (read-line *query-io*)))
   (when (zerop (length dir)) (return))
   (add-catalog *server* dir)))
