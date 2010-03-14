(defpackage :com.nklein.parser-generator.types
  (:use :common-lisp)
  (:export #:pg-array-element
	     #:type
	     #:from
	   #:pg-array
	     #:min-elements
	     #:max-elements
	     #:element-types
	   #:pg-field
	     #:name
	     #:from
	     #:type
	     #:default
	     #:optional
	   #:pg-struct
	     #:name
	     #:extends
	     #:struct-fields
	   #:pg-parser-generator
	     #:root
	     #:from
	     #:parsed-types))

(in-package :com.nklein.parser-generator.types)

(defclass pg-array-element ()
  ((type :initarg :type :type string)
   (from :initarg :from :type string)))

(defclass pg-array ()
  ((min-elements :initarg :min-elements :type integer :initform 0)
   (max-elements :initarg :max-elements :type integer)
   (element-types :initarg :element-types :type list :initform nil)))

(defclass pg-field ()
  ((name :initarg :name :type string)
   (type :initarg :type :type string)
   (from :initarg :from :type string :initform "")
   (default :initarg :default :type string)
   (optional :initarg :optional :type t :initform nil)))

(defclass pg-struct ()
  ((name :initarg :name :type string)
   (extends :initarg :extends :type string)
   (struct-fields :initarg :struct-fields :type list :initform nil)))

(defclass pg-parser-generator ()
  ((root :initarg :root :type string)
   (from :initarg :from :type string)
   (parsed-types :initarg :parsed-types :type list :initform nil)))
