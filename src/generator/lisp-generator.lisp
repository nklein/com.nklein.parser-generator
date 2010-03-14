(in-package :com.nklein.parser-generator)

(defclass lisp-generator (generator)
  ((prefix :initform "" :initarg :prefix)
   (types-package :initarg :types-package)))

(defgeneric get-lisp-types-filename (generator))
(defmethod get-lisp-types-filename ((generator lisp-generator))
  (with-slots (output-directory) generator
    (merge-pathnames (make-pathname :name "gen-types" :type "lisp")
		     output-directory)))

(defgeneric lisp-field-declaration (generator field))
(defgeneric lisp-field-type (generator field field-type))
(defgeneric lisp-field-initform (generator field field-type))

(defun lisp-local-name (generator name)
  (declare (ignore generator))
  (map 'string #'(lambda (char)
		   (if (member char '(#\Space #\Tab))
		       #\-
		       char))
       name))

(defun lisp-name (generator name)
  (with-slots (prefix) generator
    (lisp-local-name generator (concatenate 'string prefix name))))

(defun lisp-determine-field-type (generator field)
  (declare (ignore generator))
  (with-slots (type nested-type) field
    (cond
      ((and (slot-boundp field 'nested-type)
	    (= (length nested-type) 1)) :|pg-array|)
      ((slot-boundp field 'type) (intern type :keyword))
      (t :|string|))))


;;; =====================================================
;;; =====================================================

(defmethod lisp-field-type ((generator lisp-generator) field field-type)
  (declare (ignore generator field field-type))
  "")

(defmethod lisp-field-initform ((generator lisp-generator) field field-type)
  (declare (ignore generator field-type))
  (if (slot-boundp field 'default)
      (format nil " :initform ~S" (slot-value field 'default))
      ""))
	      
;;; =====================================================
;;; =====================================================

(defmethod lisp-field-type ((generator lisp-generator)
			    field
			    (field-type (eql :|string|)))
  (declare (ignore generator field field-type))
  " :type string")

(defmethod lisp-field-type ((generator lisp-generator)
			    field
			    (field-type (eql :|boolean|)))
  (declare (ignore generator field field-type))
  " :type t")

(defmethod lisp-field-initform ((generator lisp-generator)
			    field
			    (field-type (eql :|boolean|)))
  (declare (ignore generator field-type))
  (if (and (slot-boundp field 'default)
	   (member (slot-value field 'default)
		   '("yes" "t" "true" "1")
		   :test #'equal))
      (format nil " :initform t")
      (format nil " :initform nil")))

(defmethod lisp-field-type ((generator lisp-generator)
			    field
			    (field-type (eql :|integer|)))
  (declare (ignore generator field field-type))
  " :type integer")

(defmethod lisp-field-initform ((generator lisp-generator)
			    field
			    (field-type (eql :|integer|)))
  (declare (ignore generator field-type))
  (if (slot-boundp field 'default)
      (format nil " :initform ~S" (parse-integer (slot-value field 'default)))
      ""))

(defmethod lisp-field-type ((generator lisp-generator)
			    field
			    (field-type (eql :|pg-array|)))
  (declare (ignore generator field field-type))
  " :type list")

(defmethod lisp-field-initform ((generator lisp-generator)
				field
				(field-type (eql :|pg-array|)))
  (declare (ignore generator field-type))
  " :initform nil")

(defmethod lisp-field-initform ((generator lisp-generator)
				field
				(field-type (eql :|pg-struct|)))
  (declare (ignore generator field field-type))
  "")

;;; =====================================================
;;; =====================================================

(defun generate-defpackage (generator info)
  (with-slots (types-package) generator
    (format t "(defpackage :~A~%" types-package)
    (format t "  (:use :common-lisp)~%")
    (format t "  (:export")
    (with-slots (parsed-types) info
      (let ((indent " "))
	(mapc #'(lambda (struct)
		  (format t indent)
		  (with-slots (name struct-fields) struct
		    (format t "#:~A" (lisp-name generator name))
		    (mapc #'(lambda (field)
			      (with-slots (name) field
				(format t "~%             #:~A"
					(lisp-local-name generator name))))
			  struct-fields))
		  (setf indent "~%           "))
	    parsed-types)))
    (format t "))~%~%")
    (format t "(in-package :~A)~%" types-package)))

(defun lisp-field-declaration (generator field)
  (with-slots (name type) field
    (let ((field-name (lisp-local-name generator name))
	  (field-type (lisp-determine-field-type generator field)))
      (format t "(~A :initarg ~:*:~A~A~A)"
	         field-name
		 (lisp-field-type generator field field-type)
		 (lisp-field-initform generator field field-type)))))

(defgeneric generate-data-type (generator name info))
(defmethod generate-data-type ((generator lisp-generator)
			       name (info pg-struct))
  (format t "~%(defclass ~A ()~%" (lisp-name generator name))
  (format t "  (")
  (with-slots (struct-fields) info
    (let ((indent ""))
      (mapc #'(lambda (field)
		(format t indent)
		(lisp-field-declaration generator field)
		(setf indent "~%   "))
	    struct-fields)))
  (format t "))~%"))

(defun lisp-generator (generator-info output-directory
		       &key prefix types-package)

  (let ((generator (make-instance 'lisp-generator
				  :generator-info generator-info
				  :output-directory output-directory
				  :prefix prefix
				  :types-package types-package)))
    (let ((lisp-types-filename (get-lisp-types-filename generator)))
      (with-open-file (*standard-output* lisp-types-filename
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
	(generate-defpackage generator generator-info)
	(with-slots (parsed-types) generator-info
	  (mapc #'(lambda (info)
		       (generate-data-type generator
					   (slot-value info 'name)
					   info))
		parsed-types))))))
