(defpackage :com.nklein.parser-generator.reader
  (:use :com.nklein.parser-generator.types :common-lisp)
  (:export #:parse))

(in-package :com.nklein.parser-generator.reader)

;;; =================================================================
;;; boiler-plate cxml handler stuff
;;; =================================================================
(defclass sax-handler (sax:default-handler)
  ((root-path :initform nil :initarg :root-path)
   (root-type :initform nil :initarg :root-type)
   (items :initform nil)
   (buffers :initform nil)
   (paths :initform nil)
   (last-item :initform nil)))

(defmethod initialize-instance :after ((handler sax-handler) &key)
  (with-slots (root-path) handler
    (setf root-path (intern root-path :keyword))))

(defgeneric start (handler item path)
  (:documentation "This is called at the opening of each xml tag")
  (:method-combination progn)
  (:method progn (handler item path)
	   (declare (ignore handler item path))))

(defgeneric data (handler item path value)
  (:documentation "This is called with attributes and text contents of tags")
  (:method-combination progn)
  (:method progn (handler item path value)
	   (declare (ignore handler item path value))))

(defgeneric end (handler item path)
  (:documentation "This is called at the closing of each xml tag")
  (:method-combination progn)
  (:method progn (handler item path)
	   (declare (ignore handler item path))))

(defun add-to-path (initial separator new)
  (intern (concatenate 'string (when initial (symbol-name initial))
		               separator
			       new)
	  :keyword))

(defun push-path (handler separator new)
  (with-slots (paths) handler
    (push (add-to-path (first paths) separator new) paths)))

(defmethod sax:start-element ((handler sax-handler)
			      namespace-uri
			      local-name
			      qname
			      attributes)
  (declare (ignore namespace-uri qname))
  (with-slots (root-path root-type paths items buffers) handler
    (push-path handler "/" local-name)
    (when (and (null items) (eql (first paths) root-path))
      (push nil paths)
      (push (make-instance root-type) items))
    (push nil buffers)
    (start handler (first items) (first paths))
    (dolist (attr attributes)
      (with-accessors ((attr-name sax:attribute-local-name)
		       (attr-value sax:attribute-value)) attr
	(data handler (first items)
	              (add-to-path (first paths) "@" attr-name)
		      attr-value)))))

(defmethod sax:characters ((handler sax-handler) data)
  (with-slots (buffers) handler
    (push data (first buffers))))

(defmethod sax:end-element ((handler sax-handler)
			    namespace-uri
			    local-name
			    qname)
  (declare (ignore namespace-uri local-name qname))
  (with-slots (root-path items paths buffers last-item) handler
    (let ((text-contents (apply #'concatenate 'string
				              (nreverse (pop buffers)))))
      (data handler (first items)
	            (add-to-path (first paths) "/" ".")
		    text-contents))
    (pop paths)
    (when (eql (first paths) root-path)
      (setf last-item (pop items)))
    (end handler (second items) (first paths))))

(defmethod sax:end-document ((handler sax-handler))
  (with-slots (last-item) handler
    last-item))

(defun parse (source &key (root-path "/parser_generator")
	                  (root-type 'pg-parser-generator))

  (let ((handler (make-instance 'sax-handler :root-path root-path
			                     :root-type root-type)))
    (typecase source
      (pathname (with-open-file (stream source :element-type 'unsigned-byte)
		  (cxml:parse-stream stream handler)))
      (t (cxml:parse source handler)))))

;;; =================================================================
;;; pg-array-element struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item pg-array-element) path value)
  (with-slots (type from) item
    (case path
      (:|@type| (setf type value))
      (:|@from| (setf from value)))))

;;; =================================================================
;;; pg-array struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item pg-array) path value)
  (with-slots (min-elements max-elements) item
    (case path
      (:|@min-elements| (setf min-elements (parse-integer value)))
      (:|@max-elements| (setf max-elements (parse-integer value))))))

(defmethod start progn ((handler sax-handler) (item pg-array) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/array_element| (push nil paths)
	                 (push (make-instance 'pg-array-element) items)))))

(defmethod end progn ((handler sax-handler) (item pg-array) path)
  (with-slots (paths items) handler
    (case path
      (:|/array_element| (pop paths)
	                  (with-slots (element-types) item
			    (setf element-types
				  (append element-types
					  (list (pop items)))))))))

;;; =================================================================
;;; pg-field struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item pg-field) path value)
  (with-slots (name type from default optional) item
    (case path
      (:|@name| (setf name value))
      (:|@type| (setf type value))
      (:|@from| (setf from value))
      (:|@default| (setf default value))
      (:|@optional| (setf optional (member (string-downcase value)
					   '("yes" "t" "true" "1")
					   :test #'equal))))))

(defmethod start progn ((handler sax-handler) (item pg-field) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/array| (push nil paths)
	         (push (make-instance 'pg-array) items)))))

(defmethod end progn ((handler sax-handler) (item pg-field) path)
  (with-slots (paths items) handler
    (case path
      (:|/array| (pop paths)
	         (with-slots (nested-type) item
		   (setf nested-type
			 (append nested-type
				 (list (pop items)))))))))

;;; =================================================================
;;; pg-struct struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item pg-struct) path value)
  (with-slots (name extends) item
    (case path
      (:|@name| (setf name value))
      (:|@extends| (setf extends value)))))

(defmethod start progn ((handler sax-handler) (item pg-struct) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/field| (push nil paths)
	         (push (make-instance 'pg-field) items)))))

(defmethod end progn ((handler sax-handler) (item pg-struct) path)
  (with-slots (paths items) handler
    (case path
      (:|/field| (pop paths)
	         (with-slots (struct-fields) item
		   (setf struct-fields
			 (append struct-fields
				 (list (pop items)))))))))

;;; =================================================================
;;; pg-parser-generator struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item pg-parser-generator) path value)
  (with-slots (root type) item
    (case path
      (:|@root| (setf root value))
      (:|@type| (setf type value)))))

(defmethod start progn ((handler sax-handler) (item pg-parser-generator) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/struct| (push nil paths)
	          (push (make-instance 'pg-struct) items)))))

(defmethod end progn ((handler sax-handler) (item pg-parser-generator) path)
  (with-slots (paths items) handler
    (case path
      (:|/struct| (pop paths)
	          (with-slots (parsed-types) item
		    (setf parsed-types
			  (append parsed-types
				  (list (pop items)))))))))
