(in-package :agraph-http-client)

(defparameter *utf8* (excl:crlf-base-ef :utf-8))

(define-condition request-failed (simple-error)
  ((response-code :initarg :response :reader @response)))

(defmethod print-object ((err request-failed) stream)
  (format stream "Request failed (status ~a): " (@response err))
  (call-next-method))

(defun boolstr (val)
  (if val "true" "false"))

(defun urlenc (&rest pairs)
  (query-to-form-urlencoded
   (loop :for (name val) :on pairs :by #'cddr
         :append (etypecase val
                   (null nil)
                   (cons (mapcar (lambda (v) (cons name v)) val))
                   (string (list (cons name val)))))))

(defclass http-service ()
  ((url-prefix :initarg :url :reader @url-prefix)
   (auth :initarg :auth :initform nil :accessor @auth)))

(defun make-request (service method url &optional body (accept "*/*") content-type)
  (let ((post-body (member method '(:post :put)))
        (*default-external-format* *utf8*)
        (headers ()))
    (setf url (concatenate 'string (@url-prefix service) url))
    (when (and (not post-body) body)
      (setf url (concatenate 'string url "?" body)
            body nil))
    (when (and body content-type)
      (push (cons :content-type content-type) headers))
    (multiple-value-bind (response code)
        (do-http-request url :method method :accept accept :content body
                         :content-type content-type :basic-authorization (@auth service)
                         :headers headers)
      (values code response))))

(defmethod json-request (srv method url &optional body (content-type "application/x-www-form-urlencoded"))
  (multiple-value-bind (code body) (make-request srv method url body "application/json" content-type)
    (if (= code 200)
        (read-json body)
        (error 'request-failed :format-control body :response code))))

(defmethod null-request (srv method url &optional body (content-type "application/x-www-form-urlencoded"))
  (multiple-value-bind (code body) (make-request srv method url body "application/json" content-type)
    (unless (= code 200)
      (error 'request-failed :format-control body :response code))
    (values)))