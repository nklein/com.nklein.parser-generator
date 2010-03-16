(in-package :com.nklein.parser-generator)

(defclass lisp-generator (generator)
  ((prefix :initform "" :initarg :prefix)
   (types-package :initarg :types-package)
   (reader-package :initarg :reader-package)))

(defun get-lisp-types-filename (generator)
  (with-slots (output-directory) generator
    (merge-pathnames (make-pathname :name "types" :type "lisp")
                     output-directory)))

(defun get-lisp-reader-filename (generator)
  (with-slots (output-directory) generator
    (merge-pathnames (make-pathname :name "reader" :type "lisp")
                     output-directory)))

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
  (with-slots (types-package reader-package) generator
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
            parsed-types)
	(when (equalp types-package reader-package)
	  (format t indent)
	  (format t "#:parse"))))
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
;;; ========================================================

(defun generate-cxml-boilerplate (generator)
  (with-slots (types-package reader-package) generator
    (unless (equalp types-package reader-package)
      (format t "(defpackage :~A~%" reader-package)
      (format t "  (:use :~A :common-lisp)~%" types-package)
      (format t "  (:export #:parse))~%~%"))
    (format t "(in-package :~A)~%" reader-package))

  (write-string "
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
  (:documentation \"This is called at the opening of each xml tag\")
  (:method-combination progn)
  (:method progn (handler item path)
           (declare (ignore handler item path))))

(defgeneric data (handler item path value)
  (:documentation \"This is called with attributes and text contents of tags\")
  (:method-combination progn)
  (:method progn (handler item path value)
           (declare (ignore handler item path value))))

(defgeneric end (handler item path)
  (:documentation \"This is called at the closing of each xml tag\")
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
    (push-path handler \"/\" local-name)
    (when (and (null items) (eql (first paths) root-path))
      (push nil paths)
      (push (make-instance root-type) items))
    (push nil buffers)
    (start handler (first items) (first paths))
    (dolist (attr attributes)
      (with-accessors ((attr-name sax:attribute-local-name)
                       (attr-value sax:attribute-value)) attr
        (data handler (first items)
                      (add-to-path (first paths) \"@\" attr-name)
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
                    (add-to-path (first paths) \"/\" \".\")
                    text-contents))
    (pop paths)
    (when (eql (first paths) root-path)
      (setf last-item (pop items)))
    (end handler (second items) (first paths))))

(defmethod sax:end-document ((handler sax-handler))
  (with-slots (last-item) handler
    last-item))

"))


(defmethod generate-parse-function (generator)
  (with-slots (generator-info) generator
    (with-slots (root from) generator-info
      (format t "(defun parse (source &key (root-path ~S)~%" from)
      (format t "                          (root-type '~A))~%"
                (lisp-name generator root))))
  (write-string "
  (let ((handler (make-instance 'sax-handler :root-path root-path
                                             :root-type root-type)))
    (typecase source
      (pathname (with-open-file (stream source :element-type 'unsigned-byte)
                  (cxml:parse-stream stream handler)))
      (t (cxml:parse source handler)))))

"))

;;; ========================================================
;;; ========================================================

(defun find-parsed-type (generator type-name)
  (with-slots (generator-info) generator
    (with-slots (parsed-types) generator-info
      (find type-name parsed-types
            :test #'equalp
            :key #'(lambda (parsed-type)
                     (with-slots (name) parsed-type
                       name))))))

(defun get-base-type (generator type)
  (let ((parsed-type (find-parsed-type generator type)))
    (cond
      ((and parsed-type
            (slot-boundp parsed-type 'extends))
                    (with-slots (extends) parsed-type
                      (get-base-type generator extends)))
      (t type))))

(defun is-simple (generator field)
  (with-slots (type nested-type) field
    (and (not nested-type)
         (member (get-base-type generator type)
                 '("string" "integer" "boolean")
                 :test #'equalp))))

(defgeneric get-assertions (generator type-or-info))

(defmethod get-assertions (generator (info pg-array))
  (with-slots (min-elements max-elements) info
    (append (when (slot-boundp info 'min-elements)
              (format nil "(assert (<= ~A (length value)))" min-elements))
            (when (slot-boundp info 'max-elements)
              (format nil "(assert (<= (length value) ~A))" max-elements)))))

(defmethod get-assertions (generator (info pg-struct))
  (with-slots (extends struct-fields) info
    (apply #'append
           (when (slot-boundp info 'extends)
             (get-assertions generator extends))
           (mapcar #'(lambda (field)
                       (with-slots (optional name) field
                         (if optional
                             nil
                             (list "(assert (slot-boundp value '"
                                   name
                                   (format nil "))~%")))))
                   struct-fields))))

(defmethod get-assertions (generator type)
  (let ((parsed-type (find-parsed-type generator type)))
    (when parsed-type
      (get-assertions generator parsed-type))))

(defun generate-value-convertor (generator name type)
  (labels ((get-basic-convertor (generator name type)
             (case (intern (get-base-type generator type) :keyword)
               (:|integer|
                   (format nil "(setf ~A (parse-integer value))"
                                         (lisp-local-name generator name)))
               (:|boolean|
                   (format nil "(setf ~A (member (string-downcase value)~%                                              '(\"yes\" \"t\" \"true\" \"1\")~%                                              :test #'equalp))"
                                         (lisp-local-name generator name)))
               (t  (format nil "(setf ~A value)"
                                         (lisp-local-name generator name)))))
           (print-assertion (assertion)
             (format nil "~%                                ~A" assertion)))
    (let ((basic (get-basic-convertor generator name type))
          (assertions (get-assertions generator type)))
      (if (null assertions)
          (format nil "~A" basic)
          (apply #'concatenate 'string
                               (format nil "(prog1 ~A" basic)
                               (append (mapcar #'print-assertion assertions)
                                       (list (format nil ")~%"))))))))

(defgeneric generate-data-case-line (generator info field))
(defmethod generate-data-case-line ((generator lisp-generator)
                                    (info pg-struct)
                                    (field pg-field))
  (with-slots (name from type nested-type) field
    (let ((actual-type (or (first nested-type) type)))
      (format t "~%      (:|~A| ~A)" from
                                     (generate-value-convertor generator
                                                               name
                                                               actual-type)))))

(defun generate-data-handler (generator info simple-types)
  (with-slots (name) info
    (format t "(defmethod data progn ((handler sax-handler)")
    (format t " (item ~A) path value)~%" (lisp-name generator name))
    (format t "  (with-slots (~A) item~%"
              (let ((indent ""))
                (apply #'concatenate 'string
                       (mapcar #'(lambda (field)
                                   (prog1
                                       (with-slots (name) field
                                         (format nil "~A~A"
                                                     indent
                                                     (lisp-local-name
                                                        generator name)))
                                     (setf indent " ")))
                               simple-types))))
    (format t "    (case path")
    (mapc #'(lambda (field)
              (generate-data-case-line generator info field))
          simple-types)
    (format t ")))~%~%")))

(defgeneric generate-start-handler (generator field type))

(defmethod generate-start-handler ((generator lisp-generator)
                                   field
                                   (type string))
  (with-slots (from type) field
    (format t "~%")
    (format t "      (:|~A|~%" from)
    (format t "             (push nil paths)~%")
    (format t "             (push (make-instance '~A) items))"
            (lisp-name generator type))))

(defmethod generate-start-handler ((generator lisp-generator) field
                                   (type pg-array))
  (declare (ignore field))
  (with-slots (element-types) type
    (mapc #'(lambda (element)
              (with-slots (type) element
                (generate-start-handler generator element type)))
          element-types)))

(defgeneric generate-end-handler (generator field field-type element-type))

(defmethod generate-end-handler ((generator lisp-generator)
                                   field
                                   (field-type pg-array)
                                   (element-type pg-array-element))
  (declare (ignore field-type))
  (with-slots (name) field
    (with-slots (from type) element-type
      (let ((name (lisp-local-name generator name)))
        (format t "~%")
        (format t "      (:|~A|~%" from)
        (format t "             (pop paths)~%")
        (format t "             (with-slots (~A) item~%" name)
        (format t "               (setf ~A~%" name)
        (format t "                     (append ~A (list (pop items))))))"
                name)))))

#|
(defmethod end progn ((handler sax-handler) (item pg-field) path)
  (with-slots (paths items) handler
    (case path
      (:|/array|
             (pop paths)
             (with-slots (nested-type) item
               (setf nested-type
                     (append nested-type (list (pop items)))))))))

|#

(defmethod generate-end-handler ((generator lisp-generator) field
                                   (field-type pg-array)
                                   (element-type (eql nil)))
  (with-slots (element-types) field-type
    (mapc #'(lambda (element)
              (generate-end-handler generator field
                                    field-type
                                    element))
          element-types)))

(defun generate-element-handler (generator info complex-types)
  (with-slots (name) info
    (format t "(defmethod start progn ((handler sax-handler)")
    (format t " (item ~A) path)~%" (lisp-name generator name))
    (format t "  (declare (ignore item))~%")
    (format t "  (with-slots (paths items) handler~%")
    (format t "    (case path")
    (mapc #'(lambda (field)
              (with-slots (type nested-type) field
                (generate-start-handler generator
                                        field
                                        (or (first nested-type) type))))
          complex-types)
    (format t ")))~%")
    (format t "~%")
    (format t "(defmethod end progn ((handler sax-handler)")
    (format t " (item ~A) path)~%" (lisp-name generator name))
    (format t "  (with-slots (paths items) handler~%")
    (format t "    (case path")
    (mapc #'(lambda (field)
              (with-slots (type nested-type) field
                (generate-end-handler generator
                                      field
                                      (or (first nested-type) type)
                                      nil)))
          complex-types)
    (format t ")))~%~%")))


(defgeneric generate-data-type-parser (generator name info))
(defmethod generate-data-type-parser ((generator lisp-generator)
                                      name (info pg-struct))
  (format t ";;; =================================================================~%" )
  (format t ";;; ~A struct~%" (lisp-name generator name))
  (format t ";;; =================================================================~%" )
  (with-slots (struct-fields) info
    (let ((simples (remove-if-not #'(lambda (field)
                                      (is-simple generator field))
                                  struct-fields))
          (complexes (remove-if #'(lambda (field)
                                    (is-simple generator field))
                                struct-fields)))
      (when simples (generate-data-handler generator info simples))
      (when complexes (generate-element-handler generator info complexes)))))

(defun lisp-generator (generator-info output-directory &optional args)
  (let* ((option-list '(("prefix" :optional)
                        ("types-package" :required)
                        ("reader-package" :required)))

         (options (nth-value 1 (getopt:getopt args option-list)))

         (prefix (or (cdr (assoc "prefix" options :test #'equal))
                     ""))

         (types-package (cdr (assoc "types-package" options :test #'equal)))
         (reader-package (cdr (assoc "reader-package" options :test #'equal))))
                            
    (let ((generator (make-instance 'lisp-generator
                                    :generator-info generator-info
                                    :output-directory output-directory
                                    :prefix prefix
                                    :types-package types-package
                                    :reader-package reader-package)))
      (let ((lisp-types-filename (get-lisp-types-filename generator))
            (lisp-reader-filename (get-lisp-reader-filename generator)))
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
                  parsed-types)))
        (with-open-file (*standard-output* lisp-reader-filename
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
          (generate-cxml-boilerplate generator)
          (generate-parse-function generator)
          (with-slots (parsed-types) generator-info
            (mapc #'(lambda (info)
                      (generate-data-type-parser generator
                                                 (slot-value info 'name)
                                                 info))
                  parsed-types)))
        (list lisp-types-filename lisp-reader-filename)))))
