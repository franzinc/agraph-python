;; TODO take overflowing cache into account, look for other blatant mistakes

(in-package :agraph-http-server)

(defconstant *upi-field-delimiter* (code-char #x07))
(defconstant *upi-record-delimiter* (code-char #x08))

(defparameter *upi-cache-alphabet*
  (coerce (loop for code from (char-code #\!) to (char-code #\~) collect
                (code-char code)) 'simple-array))

(defstruct (stream-upi-cache (:print-object print-stream-upi-cache))
  ;; start at 1 to account for 'empty' entry
  (next-index 1)
  (dictionary (db.quicktab:create-quicktab 100)))

(defun write-code-to-stream (code stream)
  (multiple-value-bind (first second) (floor (1+ code) (length *upi-cache-alphabet*))
    (write-char (svref *upi-cache-alphabet* (1+ first)) stream)
    (write-char (svref *upi-cache-alphabet* second) stream)))

(defun write-control-code-to-stream (code stream)
;  (assert (< code (length *upi-cache-alphabet*)))
  (write-char #\! stream)
  (write-char (svref *upi-cache-alphabet* code) stream))

(defun stream-upi-with-cache (string-table upi stream cache)
  (let ((*print-pretty* nil))
    (if (hashed-upi-p upi)
        (multiple-value-bind (id local-name) (triple-store::upi-to-namespace-id-and-local-name string-table upi)
          (cond ((= (upi-type-code upi) +rdf-literal+)
                 (write-code-to-stream 0 stream))
                (id
                 (let ((code (db.quicktab:getquick id (stream-upi-cache-dictionary cache))))
                   (unless code
                     ;; not found, add and stream
                     (multiple-value-bind (namespace extra)
                         (triple-store::string-table-read-data-at-position string-table id)
                       (assert (null extra))
                       (ecase (upi-type-code upi)
                         (#.+rdf-resource+ (write-control-code-to-stream 1 stream))
                         (#.+rdf-literal-typed+ (write-control-code-to-stream 2 stream))
                         (#.+rdf-literal-language+ (write-control-code-to-stream 3 stream)))
                       (write-code-to-stream (setf code (incf (stream-upi-cache-next-index cache)))
                                             stream)
                       (setf (db.quicktab:getquick id (stream-upi-cache-dictionary cache)) code)
                       (princ namespace stream)
                       (write-char *upi-field-delimiter* stream)))
                   (write-code-to-stream code stream))))
          (princ local-name stream))
        (progn (write-code-to-stream 0 stream)
               (princ (triple-store::%upi->string *db* upi) stream)))))

(defwriter (:query-cursor "application/x-sparql-compact") (tcursor stream)
  (destructuring-bind (cursor names) tcursor
    (let ((cache (make-stream-upi-cache))
          (st (string-table *db*))
          (size (length names)))
      (flet ((write-row (row)
               (loop :for i :from 0 :below size
                     :do (stream-upi-with-cache st (svref row i) stream cache)
                     :do (write-char (if (= i (1- size)) *upi-record-delimiter* *upi-field-delimiter*) stream))))
        (write-row (map 'simple-array (lambda (v) (upi (literal v))) names))
        (loop :for row := (sparql.algebra:yield-bindings cursor) :while row
              :do (write-row row))))))
