(in-package :agraph-http-client)

(defparameter *utf8* (excl:crlf-base-ef :utf-8))

(defun boolstr (val)
  (if val "true" "false"))

(defun urlenc (&rest pairs)
  (if (cdr pairs)
      (query-to-form-urlencoded
       (loop :for (name val) :on pairs :by #'cddr
             :append (etypecase val
                       (null nil)
                       (cons (mapcar (lambda (v) (cons name v)) val))
                       (string (list (cons name val))))))
      (net.aserve::encode-form-urlencoded (car pairs) :external-format *utf8*)))

(defun read-text-file (file &optional (external-format *utf8*))
  "Slurp a text file into a string."
  (with-open-file (input file :direction :input)
    (setf (stream-external-format input) external-format)
    (let* ((buffer-size 2048)
           (parts (loop :for buffer := (make-string buffer-size)
                        :for bytes := (read-sequence buffer input)
                        :until (zerop bytes)
                        :collect (if (= bytes buffer-size) buffer (subseq buffer 0 bytes)))))
      (apply #'concatenate 'string parts))))
