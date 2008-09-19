(in-package :agraph-http-server)

(defgeneric fields (cursor))
(defgeneric get-row (cursor))

(defstruct algebra-row-cursor names cursor vector)

(defun wrap-algebra-row-cursor (cursor names)
  (make-algebra-row-cursor :names names :cursor cursor :vector (make-array (length names))))

(defmethod fields ((cursor algebra-row-cursor))
  (algebra-row-cursor-names cursor))

(defmethod get-row ((cursor algebra-row-cursor))
  (let ((row (sparql.algebra:yield-bindings (algebra-row-cursor-cursor cursor)))
        (out (algebra-row-cursor-vector cursor)))
    (when row
      (loop :for i :below (length out)
            :for val :across row
            :do (setf (svref out i) val))
      out)))

(defstruct list-cursor list names vector)

(defun wrap-list-cursor (list names)
  (make-list-cursor :list list :names names :vector (make-array (length names))))

(defmethod fields ((cursor list-cursor))
  (list-cursor-names cursor))

(defmethod get-row ((cursor list-cursor))
  (let ((row (pop (list-cursor-list cursor)))
        (out (list-cursor-vector cursor)))
    (when row
      (loop :for i :below (length out)
            :for val :in row
            :do (setf (svref out i) val))
      out)))

(defstruct algebra-triple-cursor cursor triple)

(defun wrap-algebra-triple-cursor (cursor)
  (let ((triple (make-triple)))
    (setf (graph triple) (default-graph-upi *db*))
    (make-algebra-triple-cursor :cursor cursor :triple triple)))

(defmethod get-row ((cursor algebra-triple-cursor))
  (let ((triple (algebra-triple-cursor-triple cursor))
        (row (sparql.algebra:yield-bindings (algebra-triple-cursor-cursor cursor))))
    (when row
      (setf (subject triple) (svref row 0)
            (predicate triple) (svref row 1)
            (object triple) (svref row 2))
      triple)))

(defmethod get-row ((cursor db.agraph::abstract-cursor))
  (cursor-next-row cursor))

(defstruct appending-cursor cursors names)

(defun append-cursors (cursors &optional names)
  (make-appending-cursor :cursors cursors :names names))

(defmethod field ((cursor appending-cursor))
  (appending-cursor-names cursor))

(defmethod get-row ((cursor appending-cursor))
  (loop :with row := nil
        :while (and (not row) (appending-cursor-cursors cursor))
        :do (setf row (get-row (car (appending-cursor-cursors cursor))))
        :unless row :do (pop (appending-cursor-cursors cursor))
        :finally (return row)))
