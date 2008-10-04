(in-package :agraph-http-server)

(defvar *server* (create-server :port 8080))
(add-catalog *server* "/home/marijn/src/lisp/agraph-browser/")

(net.aserve::debug-on :notrap)
(setf net.aserve::*enable-logging* nil)
