(in-package :agraph-http-server)

(setf *catalog* (make-instance 'published-catalog :directory "/home/marijn/src/lisp/agraph-browser/"))
(start :port 8080)
(publish-catalog *wserver* *catalog*)

(net.aserve::debug-on :notrap)
(setf net.aserve::*enable-logging* nil)
