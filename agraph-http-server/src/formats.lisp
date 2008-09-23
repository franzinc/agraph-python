(in-package :agraph-http-server)

(defparameter *format-writers* (make-hash-table))

(defun register-writer (value-format mime function)
  (setf (gethash value-format *format-writers*)
        (cons (cons mime function)
              (remove-if (lambda (wr) (string= (car wr) mime))
                         (gethash value-format *format-writers*)))))

(defmacro defwriter ((value-format mime) (val stream) &body body)
  (unless (consp value-format) (setf value-format (list value-format)))
  `(dolist (format ',(if (consp value-format) value-format (list value-format)))
     (register-writer format ,mime (lambda (,val ,stream) ,@body))))

(defun find-format-writer (value-format accepted)
  (let ((writers (gethash value-format *format-writers*)))
    (dolist (mime accepted)
      (when (eq mime t) (return (car writers)))
      (let ((found (find mime writers :test #'string= :key #'car)))
        (when found (return found))))))
        
(defwriter (:list "text/plain") (lst stream)
  (format stream "~{~a~%}" lst))

(defwriter (:boolean "text/plain") (bool stream)
  (princ (if bool "True" "False") stream))

(defwriter (:null "text/plain") (null stream)
  (declare (ignore null stream)))

(defwriter ((:string :integer :float) "text/plain") (val stream)
  (princ val stream))

(defwriter ((:list :integer :string :float :namespaces) "application/json") (val stream)
  (write-json val stream))

(defwriter (:boolean "application/json") (bool stream)
  (write-json (as-json-bool bool) stream))

(defwriter (:null "application/json") (null stream)
  (declare (ignore null))
  (princ "null" stream))

(defwriter (:row-cursor "application/json") (cursor stream)
  (format stream "{\"names\":~a,\"values\":[" (write-json-to-string (fields cursor)))
  (loop :for row := (get-row cursor) :while row
        :for first := t :then nil
        :unless first :do (write-char #\, stream)
        :do (write-json row stream))
  (princ "]}" stream))

(defwriter (:triple-cursor "application/json") (cursor stream)
  (write-char #\[ stream)
  (loop :for triple := (get-row cursor) :while triple
        :for first := t :then nil
        :unless first :do (write-char #\, stream)
        :do (write-json triple stream))
  (write-char #\] stream))

(defwriter (:triple-cursor "text/plain") (cursor stream)
  (loop :for triple := (get-row cursor) :while triple
        :do (db.agraph::print-triple-to-stream-ntriples triple stream)))

;; This should probably be centralised somewhere.
(defwriter (:row-cursor "application/sparql-results+xml") (cursor stream)
  (let ((names (fields cursor)))
    (princ "<?xml version=\"1.0\"?><sparql xmlns=\"http://www.w3.org/2005/sparql-results#\">" stream)
    (format stream "<head>~{<variable name=\"~a\"/>~}</head>" names)
    (princ "<results>" stream)
    (loop :for row := (get-row cursor) :while row
          :do (princ "<result>" stream)
          :do (loop :for name :in names :for term :across row
                    :do (format stream "<binding name=\"~a\">~a</binding>" name
                                (with-output-to-string (sparql.results::*output-stream*)
                                  (sparql.results::write-sparql-xml-var term))))
          :do (princ "</result>" stream))
    (princ "</results></sparql>" stream)))
