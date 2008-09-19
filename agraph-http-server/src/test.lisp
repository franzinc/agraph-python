(in-package :agraph-http-server)

(setf *server* (make-instance 'simple-server))
(start :port 8080)
(publish-simple-server *wserver* *server*)
(server-add-store *server* "foo" :file "/home/marijn/src/lisp/agraph-browser/kennedy")

(net.aserve::debug-on :notrap)
(setf net.aserve::*enable-logging* nil)
