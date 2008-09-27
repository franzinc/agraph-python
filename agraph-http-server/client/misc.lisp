(in-package :agraph-http-client)

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
