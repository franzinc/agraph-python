;; $Id: main.lisp,v 1.3 2008/10/11 21:34:15 layer Exp $

(in-package :agraph-http-server)

(defvar *server* nil)

#+mswindows
(error "does not work on Windows yet.")

(defun start-python-http-server (port directories)
  (setf net.aserve::*enable-logging* nil)
  (let ((port (or port 8080)))
    (setq *server* (create-server :port port))
    (format t "~%~%Running at port ~a." port))
  (dolist (dir directories)
    (add-catalog *server* dir)
    (format t "~%Publishing directory '~a'.~%" dir))
  (loop (sleep most-positive-fixnum)))

(defun python-http-server-main-1 (port directories log-file debug)
  (when (not (excl.osi:detach-from-terminal-supported-p))
    (error "Cannot daemonize."))
  
  ;; Exit on SIGINT
  (excl::add-signal-handler
   2 (lambda (sig cont)
       (excl::sig-handler-exit sig cont)))

  (unless debug
    (let ((lf (open log-file :direction :output :if-exists
		    :supersede)))
      (cond ((= (excl.osi:fork) 0)
	     (format t "Daemonizing...~%")
	     ;; child
	     (setf (file-contents "sys:server.pid")
	       (format nil "~d~%" (excl.osi:getpid)))
	     (excl.osi:detach-from-terminal :output-stream lf
					    :error-output-stream lf))
	    (t ;; parent
	     (exit 0 :quiet t)))))
  
  (start-python-http-server port directories))

(defun cl-user::python-http-server-main (&rest args)
  (sys:with-command-line-arguments
      (("port" :long port :required-companion)
       ("log" :long log-file :required-companion)
       ("D" :short debug)
       ("d" :short directories :required-companion :allow-multiple-options))
      (rest :command-line-arguments args)
    (declare (ignore rest))
    (handler-case (python-http-server-main-1
		   (when port (ignore-errors (parse-integer port)))
		   directories
		   (or log-file "sys:server.log") 
		   debug)
      (error (c)
	(format t "~&~a~&" c)
	#+mswindows
	(progn (excl::maybe-show-console-window t)
	       (console-control :close t))
	(exit -1 :quiet t)))))
