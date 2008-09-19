(in-package :agraph-http-server)

(defparameter *response-not-acceptable*
  (net.aserve::make-resp 406 "Not acceptable"))

(define-condition request-failed (simple-error)
  ((response-code :initarg :response :initform *response-bad-request* :reader @response)))
(defun request-failed (format &rest args)
  (error 'request-failed :format-control format :format-arguments args))
(defun request-failed* (response format &rest args)
  (error 'request-failed :format-control format :format-arguments args :response response))
(defun request-assert (condition format &rest args)
  (unless condition (apply 'request-failed format args)))

(defun publish-simple-server (wserver simple-server &key (prefix "/"))
  (flet ((serve (req ent)
           (simple-serve simple-server (subseq (net.uri:uri-path (request-raw-uri req)) (length prefix)) req ent)))
    (publish-prefix :prefix prefix :server wserver :function #'serve)))

(defun simple-serve (server path req ent)
  (multiple-value-bind (matches match store store-path) (match-re "^repositories/([^/]+)(?:/(.*))?$" path)
    (declare (ignore match))
    (if matches
        (dispatch-service server store (or store-path "") req ent)
        (dispatch-service server nil path req ent))))

(defun dispatch-service (server store path req ent)
  (handler-case
      (let ((service (or (find-service path (request-method req))
                         (request-failed* *response-not-found* "Not found.")))
            (*db* nil)
            (*server* server))
        (unless (or (and store (service-store-p service)) (not store))
          (request-failed* *response-not-found* "Not found."))
        (when store
          (setf *db* (or (find-store server store) 
                         (request-failed* *response-not-found* "No store '~a' known." store))))
        (multiple-value-bind (type value) (call-service service (interpret-parameters service (read-parameter req)))
          (write-response value (negotiate-format type req) req ent)))
    (request-failed (err)
      (error-response req ent (@response err) err))
    #+(or)(error (err)
      (error-response req ent *response-internal-server-error* err))))

(defun read-parameter (request)
  (lambda (name type &optional (default nil default-p))
    (case type
      (:postbody (get-request-body request))
      (:list (mapcan (lambda (pair) (and (string= (car pair) name) (list (cdr pair))))
                     (request-query request)))
      (t (let ((str (request-query-value name request)))
           (cond (str
                  (multiple-value-bind (value ok) (read-type type str)
                    (unless ok (request-failed "'~a' is not a valid ~a." str type))
                    value))
                 (default-p default)
                 (t (request-failed "No value given for argument '~a'." name))))))))

(defun parse-accept (header)
  (unless header (return-from parse-accept '(t)))
  (let ((parts (loop :for type :in (split-re ", ?" header)
                     :collect (multiple-value-bind (matches match mime rest) (match-re "^([^;]+)(.*)" type)
                                (declare (ignore match matches))
                                (let ((weight 1))
                                  (multiple-value-bind (matches match q) (match-re "\\bq=([0-9.]+)" rest)
                                    (declare (ignore match))
                                    (when matches (setf weight (or (ignore-errors (read-from-string q)) 1))))
                                  (when (find #\* mime) (setf mime t))
                                  (cons weight mime))))))
    (mapcar #'cdr (stable-sort parts #'> :key #'car))))

(defun negotiate-format (value-type request)
  (let ((accepted (parse-accept (header-slot-value request :accept))))
    (or (find-format-writer value-type accepted)
        (request-failed* *response-not-acceptable* "No suitable format available."))))

(defun write-response (value writer req ent)
  (destructuring-bind (mime . writer) writer
    (with-http-response (req ent :content-type (format nil "~a; charset=UTF-8" mime))
      (let ((encoding (cond ((accepts-deflate req) :deflate) ((accepts-gzip req) :gzip) (t nil))))
        (when encoding (setf (reply-header-slot-value req :content-encoding) (string-downcase (symbol-name encoding))))
        (with-http-body (req ent :external-format *utf8*)
          (if encoding
              (with-deflate (out (request-reply-stream req)
                                 (ecase encoding (:gzip 'salza2:gzip-compressor) (:deflate 'salza2:deflate-compressor)))
                (funcall writer value out))
              (funcall writer value (request-reply-stream req))))))))
