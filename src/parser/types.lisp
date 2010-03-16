(defpackage :com.nklein.parser-generator.types
  (:use :common-lisp)
  (:export #:pg-array-element
             #:type
             #:pg-type
             #:from
             #:pg-from
           #:pg-array
             #:min-elements
             #:pg-min-elements
             #:max-elements
             #:pg-max-elements
             #:element-types
             #:pg-element-types
           #:pg-field
             #:name
             #:pg-name
             #:type
             #:pg-type
             #:from
             #:pg-from
             #:default
             #:pg-default
             #:optional
             #:pg-optional
             #:nested-type
             #:pg-nested-type
           #:pg-struct
             #:name
             #:pg-name
             #:extends
             #:pg-extends
             #:struct-fields
             #:pg-struct-fields
           #:pg-parser-generator
             #:root
             #:pg-root
             #:from
             #:pg-from
             #:parsed-types
             #:pg-parsed-types))

(in-package :com.nklein.parser-generator.types)

(defclass pg-array-element ()
  ((type :initarg :type :accessor pg-type :type string)
   (from :initarg :from :accessor pg-from :type string)))

(defclass pg-array ()
  ((min-elements :initarg :min-elements :accessor pg-min-elements :type integer :initform 0)
   (max-elements :initarg :max-elements :accessor pg-max-elements :type integer)
   (element-types :initarg :element-types :accessor pg-element-types :type list :initform nil)))

(defclass pg-field ()
  ((name :initarg :name :accessor pg-name :type string)
   (type :initarg :type :accessor pg-type :type string)
   (from :initarg :from :accessor pg-from :type string :initform "")
   (default :initarg :default :accessor pg-default :type string)
   (optional :initarg :optional :accessor pg-optional :type t :initform nil)
   (nested-type :initarg :nested-type :accessor pg-nested-type :type list :initform nil)))

(defclass pg-struct ()
  ((name :initarg :name :accessor pg-name :type string)
   (extends :initarg :extends :accessor pg-extends :type string)
   (struct-fields :initarg :struct-fields :accessor pg-struct-fields :type list :initform nil)))

(defclass pg-parser-generator ()
  ((root :initarg :root :accessor pg-root :type string)
   (from :initarg :from :accessor pg-from :type string)
   (parsed-types :initarg :parsed-types :accessor pg-parsed-types :type list :initform nil)))
