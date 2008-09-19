;; zlib version of deflating stream. Only ~15% faster, since zipping is
;; not the major bottleneck, and much less convenient, so disabled for
;; now.

(load "libz.so")
(load "zstream.so") ;; Compile from zstream.c in this directory.

(ff:def-foreign-call (c-open-zstream "openZStream")
    ((level :int))
  :call-direct t
  :returning :int)

(ff:def-foreign-call (c-close-zstream "closeZStream")
    ((stream :long))
  :call-direct t
  :returning :int)

(ff:def-foreign-call (c-zstream-deflate "zStreamDeflate")
    ((stream :long)
     (input (* :void))
     (in-size :unsigned-int)
     (output (* :void))
     (out-size :unsigned-int)
     (flush :int))
  :strings-convert nil
  :call-direct t
  :returning :int)

(ff:def-foreign-call (c-zstream-avail-in "zStreamAvailIn")
    ((stream :long))
  :call-direct t
  :returning :unsigned-int)

(ff:def-foreign-call (c-zstream-avail-out "zStreamAvailOut")
    ((stream :long))
  :call-direct t
  :returning :unsigned-int)

(defvar *buf-size* 8192)

(defclass c-zlib-compressor ()
  ((pointer :reader @pointer)
   (buffer :initform (make-array *buf-size* :element-type '(unsigned-byte 8)) :reader @buffer)
   (callback :initarg :callback :reader @callback)))

(defmethod shared-initialize :after ((cmp c-zlib-compressor) slot-names &key (level 6) &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value cmp 'pointer) (c-open-zstream level)))

(defun write-zstream-buf (compressor buf size flush)
  (flet ((deflate (buf size)
           (let ((status (c-zstream-deflate (@pointer compressor) buf size (@buffer compressor) *buf-size* flush)))
             (unless (or (zerop status) (and (= status 1) (= flush 4)))
               (error "Error writing to zstream: ~a" status)))
           (funcall (@callback compressor) (@buffer compressor)
                    (- *buf-size* (c-zstream-avail-out (@pointer compressor))))))
    (deflate buf size)
    (loop :while (zerop (c-zstream-avail-out (@pointer compressor)))
          :do (deflate 0 0))))

(defmethod salza2:compress-octet-vector (vector (compressor c-zlib-compressor) &key (start 0) end)
  (assert (zerop start))
  (write-zstream-buf compressor vector (or end (length vector)) 0))

(defmethod salza2:finish-compression ((compressor c-zlib-compressor))
  (write-zstream-buf compressor (@buffer compressor) 0 4)
  (let ((status (c-close-zstream (@pointer compressor))))
    (unless (zerop status) (error "Failed to close zstream: ~a" status))))

(defun test-def ()
  (with-open-file (f "/tmp/foo" :direction :output :if-exists :supersede)
    (let ((out (make-instance 'compressing-stream :compressor 'c-zlib-compressor :output-handle f)))
      (dotimes (i 100) (princ "FOO" out))
      (close out))))

