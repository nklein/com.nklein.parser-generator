(in-package :com.nklein.parser-generator)

#|
(defmethod validate (handler (item pg-string))
  (with-slots (name from) item
    (assert (or (and (slot-boundp item 'name) name)
		(and (slot-boundp item 'from) from)))))

(defmethod validate (handler (item pg-unsigned-integer))
  (declare (ignore handler))
  (with-slots (name from default min max) item
    (assert (or (and (slot-boundp item 'name) name)
		(and (slot-boundp item 'from) from)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'min))
		(<= min default)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'max))
		(<= default max)))
    (assert (or (not (slot-boundp item 'min))
		(not (slot-boundp item 'max))
		(<= min max)))
    (assert (or (not (slot-boundp item 'default)) (<= 0 default)))
    (assert (or (not (slot-boundp item 'min)) (<= 0 min)))
    (assert (or (not (slot-boundp item 'max)) (<= 0 max)))))

(defmethod validate (handler (item pg-integer))
  (declare (ignore handler))
  (with-slots (name from default min max) item
    (assert (or (and (slot-boundp item 'name) name)
		(and (slot-boundp item 'from) from)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'min))
		(<= min default)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'max))
		(<= default max)))
    (assert (or (not (slot-boundp item 'min))
		(not (slot-boundp item 'max))
		(<= min max)))))

(defmethod validate (handler (item pg-real))
  (declare (ignore handler))
  (with-slots (name from default min max) item
    (assert (or (and (slot-boundp item 'name) name)
		(and (slot-boundp item 'from) from)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'min))
		(<= min default)))
    (assert (or (not (slot-boundp item 'default))
		(not (slot-boundp item 'max))
		(<= default max)))
    (assert (or (not (slot-boundp item 'min))
		(not (slot-boundp item 'max))
		(<= min max)))))

(defmethod validate (handler (item pg-struct))
  (declare (ignore handler))
  (with-slots (name from typed-fields) item
    (assert (or (and (slot-boundp item 'name) name)
		(and (slot-boundp item 'from) from)))
    (assert (and (slot-boundp item 'name) name))
    (assert (plusp (length typed-fields)))))
|#