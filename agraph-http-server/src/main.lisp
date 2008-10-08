;; $Id: main.lisp,v 1.1 2008/10/08 16:42:38 layer Exp $

(in-package :agraph-http-server)

(defvar *server* nil)

#+mswindows
(error "does not work on Windows yet.")

(defun main-1 (port directories log-file)
  (when (not (excl.osi:detach-from-terminal-supported-p))
    (error "Cannot daemonize."))
  
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
	   (exit 0 :quiet t))))  
  
  (setf net.aserve::*enable-logging* nil)
  (let ((port (or (ignore-errors (parse-integer port)) 8080)))
    (setq *server* (create-server :port port))
    (format t "~%~%Running at port ~a." port))
  (dolist (dir directories)
    (add-catalog *server* dir)
    (format t "~%Publishing directory '~a'.~%" dir))
  (loop (sleep most-positive-fixnum)))

(defun cl-user::main (&rest args)
  (sys:with-command-line-arguments
      (("port" :long port :required-companion)
       ("log" :long log-file :required-companion)
       ("d" :short directories :required-companion :allow-multiple-options))
      (rest :command-line-arguments args)
    (handler-case (main-1 port directories (or log-file "sys:server.log"))
      (error (c)
	(format t "~&~a~&" c)
	#+mswindows
	(progn (excl::maybe-show-console-window t)
	       (console-control :close t))
	(exit -1 :quiet t)))))
