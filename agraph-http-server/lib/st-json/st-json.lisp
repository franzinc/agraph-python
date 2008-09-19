(defpackage :st-json
  (:use :common-lisp)
  (:export #:read-json #:read-json-as-type
           #:write-json #:write-json-to-string #:write-json-element
           #:as-json-bool #:from-json-bool
           #:json-bool #:json-null
           #:jso #:getjso #:mapjso
           #:json-error #:json-type-error #:json-parse-error
           #:*script-tag-hack*))

(in-package :st-json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 1) (debug 1) (compilation-speed 0))))

;; Boolean types. It is hard to see what is meant by NIL when encoding
;; a lisp value -- false or [] -- so :false and :true are used instead
;; of T and NIL.
(defun as-json-bool (value)
  "Convert a generalised boolean to a :true/:false keyword."
  (if value :true :false))
(defun from-json-bool (value)
  "Convert :true or :false to its boolean equivalent."
  (ecase value (:true t) (:false nil)))

;; Types that might be useful when checking the type of input.
(deftype json-bool () '(member :true :false))
(deftype json-null () '(eql :null))

;; These are used to represent JS objects on the Lisp side -- hash
;; tables are too heavyweight on some implementations.
(defstruct jso alist)
(defun jso (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values."
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                           :collect (cons key val))))

;; A hash-table-like interface for JS objects.
(defun getjso (key map)
  "Fetch a value from a JS object. Returns a second value like
gethash."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (values (cdr pair) (and pair t))))
(defun (setf getjso) (val key map)
  "Store a value in a JS object."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (if pair
        (setf (cdr pair) val)
        (prog1 val (push (cons key val) (jso-alist map))))))
(defun mapjso (func map)
  "Iterate over the key/value pairs in a JS object."
  (loop :for (key . val) :in (jso-alist map)
        :do (funcall func key val)))

;; Reader

(define-condition json-error (simple-error) ())
(define-condition json-parse-error (json-error) ())
(define-condition json-write-error (json-error) ())
(define-condition json-type-error (json-error) ())
(defun raise (type format &rest args)
  (error type :format-control format :format-arguments args))

(defvar *reading-slot-name* nil)

(defun is-whitespace (char)
  (member char '(#\space #\newline #\return #\tab)))

(defun ends-atom (char)
  (or (is-whitespace char) (member char '(#\) #\] #\} #\, #\:))))

(defun skip-whitespace (stream)
  (declare #.*optimize*)
  (loop :while (is-whitespace (peek-char nil stream nil))
        :do (read-char stream)))

(defun at-eof (stream)
  (eql (peek-char nil stream nil :eof) :eof))

(defgeneric read-json (in &optional junk-allowed-p)
  (:documentation "Read a JSON-encoded value from a stream or a string."))

(defmethod read-json ((in stream) &optional (junk-allowed-p t))
  (let ((value (read-json-element in)))
    (skip-whitespace in)
    (unless (or junk-allowed-p (at-eof in))
      (raise 'json-parse-error "Unused characters at end of input."))
    value))

(defmethod read-json ((in string) &optional (junk-allowed-p nil))
  (with-input-from-string (stream in)
    (read-json stream junk-allowed-p)))

(defun read-json-as-type (source type)
  "Read a JSON value and assert the result to be of a given type.
Raises a json-type-error when the type is wrong."
  (let ((val (read-json source)))
    (if (typep val type)
        val
        (raise 'json-type-error "JSON input '~A' is not of expected type ~A." source type))))

(defun read-json-element (stream)
  (declare #.*optimize*)
  (skip-whitespace stream)
  (case (peek-char nil stream nil :eof)
    (:eof (raise 'json-parse-error "Unexpected end of input."))
    ((#\" #\') (read-json-string stream))
    (#\[ (read-json-list stream))
    (#\{ (read-json-object stream))
    (t (read-json-atom stream))))

(defun read-json-string (stream)
  (declare #.*optimize*)
  (labels ((interpret (char)
             (if (eql char #\\)
                 (let ((escaped (read-char stream)))
                   (case escaped
                     (#\u (read-unicode))
                     (#\b #\backspace) (#\n #\newline) (#\r #\return)
                     (#\t #\tab) (#\f #\page) (t escaped)))
                 char))
           (read-unicode ()
             (code-char (loop :for pos :from 0 :below 4
                              :for weight :of-type fixnum := #.(expt 16 3) :then (ash weight -4)
                              :for digit := (digit-char-p (read-char stream) 16)
                              :do (unless digit (raise 'json-parse-error "Invalid unicode constant in string."))
                              :sum (* digit weight)))))
    (with-output-to-string (out)
      (handler-case
          (loop :with quote :of-type character := (read-char stream)
                :for next :of-type character := (read-char stream)
                :until (eql next quote)
                :do (write-char (interpret next) out))
        (end-of-file () (raise 'json-parse-error "Encountered end of input inside string constant."))))))

(defun gather-comma-separated (stream end-char obj-name gather-func)
  (declare #.*optimize*)
  (declare (type character end-char))
  (declare (type function gather-func))
  ;; Throw away opening char
  (read-char stream)
  (let ((finished nil))
    (loop
     (skip-whitespace stream)
     (let ((next (peek-char nil stream nil #\nul)))
       (declare (type character next))
       (when (eql next #\nul)
         (raise 'json-parse-error "Encountered end of input inside ~A." obj-name))
       (when (eql next end-char)
         (read-char stream)
         (return))
       (when finished
         (raise 'json-parse-error "Comma or end of ~A expected, found '~A'" obj-name next)))
     (funcall gather-func)
     (skip-whitespace stream)
     (if (eql (peek-char nil stream nil) #\,)
         (read-char stream)
         (setf finished t)))))

(defun read-json-list (stream)
  (declare #.*optimize*)
  (let ((accum ()))
    (gather-comma-separated
     stream #\] "list"
     (lambda ()
       (push (read-json-element stream) accum)))
    (nreverse accum)))

(defun read-json-object (stream)
  (declare #.*optimize*)
  (let ((accum ()))
    (gather-comma-separated 
     stream #\} "object literal"
     (lambda ()
       (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
         (unless (or (typep slot-name 'string) (typep slot-name 'number))
           (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
         (skip-whitespace stream)
         (when (not (eql (read-char stream nil) #\:))
           (raise 'json-parse-error "Colon expected after '~a'." slot-name))
         (push (cons slot-name (read-json-element stream)) accum))))
    (make-jso :alist (nreverse accum))))

(defun looks-like-a-number (string)
  (declare #.*optimize*)
  (let ((string (coerce string 'simple-string)))
    (every (lambda (char)
             (or (digit-char-p char)
                 (member char '(#\e #\E #\. #\- #\+))))
           string)))

(defun read-json-atom (stream)
  (declare #.*optimize*)
  (let ((accum (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
     (let ((next (peek-char nil stream nil :eof)))
       (when (or (ends-atom next) (eql next :eof))
         (return))
       (vector-push-extend next accum)
       (read-char stream)))
    (let ((number-val (and (looks-like-a-number accum)
                           (ignore-errors (read-from-string accum)))))
      (cond ((numberp number-val) number-val)
            ((string= accum "false") :false)
            ((string= accum "true") :true)
            ((string= accum "null") :null)
            ((string= accum "undefined") :undefined)
            ((and *reading-slot-name*
                  (every (lambda (c)
                           (declare (type character c))
                           (or (alphanumericp c) (eql c #\_) (eql c #\$)))
                         accum))
             accum)
            (t (raise 'json-parse-error "Unrecognized value in JSON data: ~A" accum))))))

;; Writer

(defparameter *script-tag-hack* nil
  "Bind this to T when writing JSON that will be written to an HTML
  document. It prevents '</script>' from occurring in strings by
  escaping any slash following a '<' character.")

(defun write-json-to-string (element)
  "Write a value's JSON representation to a string."
  (with-output-to-string (out)
    (write-json element out)))

(defun write-json (element stream)
  "Write a value's JSON representation to a stream."
  (let ((*print-pretty* nil))
    (write-json-element element stream)
    (values)))

(defgeneric write-json-element (element stream)
  (:method (element stream)
    (declare (ignore stream))
    (raise 'json-write-error "Can not write object of type ~A as JSON." (type-of element)))
  (:documentation "Method used for writing values of a specific type.
  You can specialise this for your own types."))

(defmethod write-json-element ((element symbol) stream)
  (declare #.*optimize*)
  (ecase element
    ((nil) (princ "[]" stream))
    ((t :true) (princ "true" stream))
    (:false (princ "false" stream))
    (:undefined (princ "undefined" stream))
    (:null (princ "null" stream))))

(defmethod write-json-element ((element string) stream)
  (declare #.*optimize*)
  (let ((element (coerce element 'simple-string)))
    (princ #\" stream)
    (loop :for prev := nil :then ch
          :for ch :across element
          :do (princ
               (case ch
                 (#\\ "\\\\") (#\" "\\\"")
                 (#\backspace "\\b") (#\newline "\\n")
                 (#\return "\\r") (#\page "\\f")
                 (#\tab "\\t")
                 ;; Prevent </script> by escaping every #\/ that follows a #\<
                 (#\/ (if (and *script-tag-hack* (eql prev #\<)) "\\/" #\/))
                 (t ch))
               stream))
    (princ #\" stream)))

(defmethod write-json-element ((element integer) stream)
  (write element :stream stream))

(defmethod write-json-element ((element real) stream)
  (format stream "~,,,,,,'eE" element))

(defmethod write-json-element ((element hash-table) stream)
  (declare #.*optimize*)
  (write-json-element
   (make-jso :alist (loop :for key :being :the :hash-key :using (hash-value val) :of element
                            :collect (cons key val)))
   stream))

(defmethod write-json-element ((element jso) stream)
  (declare #.*optimize*)
  (princ #\{ stream)
  (loop :for (key . val) :in (jso-alist element)
        :for first := t :then nil
        :unless first :do (princ ", " stream)
        :do (write-json-element key stream)
        :do (princ ": " stream)
        :do (write-json-element val stream))
  (princ #\} stream))

(defmethod write-json-element ((element list) stream)
  (declare #.*optimize*)
  (princ #\[ stream)
  (let ((first t))
    (dolist (part element)
     (if first
         (setf first nil)
         (princ ", " stream))
     (write-json-element part stream)))
  (princ #\] stream))
