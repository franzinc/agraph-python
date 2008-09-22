(in-package :agraph-http-server)

(defparameter *utf8* (excl:crlf-base-ef :utf-8))

;; Error type for cleanly failing a request.

(define-condition request-failed (simple-error)
  ((response-code :initarg :response :initform *response-bad-request* :reader @response)
   (headers :initform nil :initarg :headers :reader @headers)))
(defun request-failed (format &rest args)
  (error 'request-failed :format-control format :format-arguments args))
(defun request-failed* (response format &rest args)
  (error 'request-failed :format-control format :format-arguments args :response response))
(defun request-assert (condition format &rest args)
  (unless condition (apply 'request-failed format args)))

(defun error-response (req ent response message &optional headers)
  "Write out a simple error response."
  (with-http-response (req ent :response response :content-type "text/plain; charset=UTF-8")
    (loop :for (name val) :in headers
          :do (setf (reply-header-slot-value req name) val))
    (with-http-body (req ent :external-format *utf8*)
      (princ message (request-reply-stream req)))))


(defmacro deftype* (name args &body body)
  "Hack to make it possible to define complicated CL types."
  (let ((func-name (gensym)))
    `(deftype ,name ,(cdr args)
      (let ((,func-name (gensym)))
        (setf (symbol-function ,func-name)
              (lambda (,(car args))
                ,@body))
        (list 'satisfies ,func-name)))))

(defun proper-list-p (list)
  "Test whether a value is a proper list."
  (cond ((null list) t)
        ((not (consp list)) nil)
        (t (proper-list-p (cdr list)))))

;; A type for lists with a specific element type and length.
(deftype* list-of (value element-type &optional (length '*))
  (and (proper-list-p value)
       (or (eq element-type '*)
           (every (lambda (x) (typep x element-type)) value))
       (or (eq length '*)
           (= (length value) length))))

(defun part-rep (part)
  "Map a part to its string representation. NOTE: blank nodes do not
work for federated triple stores."
  (cond ((blank-node-p part)
         (when (typep *db* 'federated-triple-store)
           (error "Can not deserialise a blank node in a federated store."))
         (format nil "_:~a" (upi->value part)))
        ((part= part (default-graph-upi *db*)) :null)
        (t (part->long part))))

(defun rep-part (rep)
  "Map a string representation back to a part. Return nil if this fails."
  (ignore-errors
    (multiple-value-bind (matches match num) (excl:match-re "^_:(\\d+)$" rep)
      (declare (ignore match))
      (if matches
          (value->upi (parse-integer num) :blank-node)
          ;; TODO Should be replaced by a function from the agraph package when
          ;; such a thing gets implemented/exported.
          (multiple-value-bind (type value extra) (triple-store::parse-part-string rep)
            (make-future-part :value value :type type :extra extra))))))

;; Custom JSON writers for triples and parts. The dispatch type is
;; dodgy -- but hey, I'm not the one who gave triples and parts such
;; generic types.
(defmethod write-json-element ((element vector) out)
  (flet ((write-part (part)
           (write-json (part-rep part) out)))
    (typecase element
      ((simple-array (unsigned-byte 8) (56)) ;; A triple, hopefully
       (write-char #\[ out)
       (write-part (subject element))
       (write-char #\, out)
       (write-part (object element))
       (write-char #\, out)
       (write-part (predicate element))
       (let ((graph (graph element)))
         (unless (part= graph (default-graph-upi *db*))
           (write-char #\, out)
           (write-part graph)))
       (write-char #\] out))
      ((simple-array (unsigned-byte 8) (12)) ;; A part
       (write-part element))
      (t (write-char #\[ out)
         (loop :for val :across element :for first := t :then nil
               :unless first :do (write-char #\, out)
               :do (write-json-element val out))
         (write-char #\] out)))))

(defmethod write-json-element ((element future-part) out)
  (write-json (part-rep element) out))

(defun intern-with-default-case (name &optional package)
  (let ((name (if (eq (readtable-case *readtable*) :upcase)
                  (string-upcase name)
                  (string-downcase name))))
    (intern name package)))

(defun accepts-encoding-pred (encoding)
  (let ((re (compile-re (format nil "\\b~a(?:;q=(\\d*\\.?\\d+))?\\b" encoding))))
    (lambda (req)
      (let ((header (header-slot-value req :accept-encoding)))
        (and header
             (multiple-value-bind (match matched q) (excl:match-re re header)
               (declare (ignore matched))
               (and match (or (not q) (> (read-from-string q) 0)))))))))

(setf (fdefinition 'accepts-gzip) (accepts-encoding-pred "gzip")
      (fdefinition 'accepts-deflate) (accepts-encoding-pred "deflate"))

(let ((abc (with-output-to-string (out)
             (loop :for ch :from (char-code #\!) :below (char-code #\~)
                   :do (princ (code-char ch) out)))))
  (defun random-string (len)
    (with-output-to-string (out)
      (dotimes (i len) (princ (elt abc (random (length abc))) out)))))
