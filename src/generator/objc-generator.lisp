(defpackage :com.nklein.parser-generator.objc-generator
  (:use :com.nklein.parser-generator.reader
	:com.nklein.parser-generator.types
	:com.nklein.parser-generator
	:common-lisp)
  (:export #:objc-generator))

(in-package :com.nklein.parser-generator.objc-generator)

(defclass objc-generator ()
  ((generator-info :initarg :generator-info
		   :initform (error "Must have generator info"))
   (output-directory :initarg :output-directory :initform #P".")
   (prefix :initform "" :initarg :prefix)
   (types-package :initarg :types-package)
   (reader-package :initarg :reader-package)))

(defun objc-make-filename (generator base type)
  (with-slots (output-directory) generator
    (merge-pathnames (make-pathname :name base :type type) output-directory)))

(defun get-objc-types-header (generator)
  (objc-make-filename generator "types" "h"))
(defun get-objc-types-impl (generator)
  (objc-make-filename generator "types" "m"))
(defun get-objc-reader-header (generator)
  (objc-make-filename generator "reader" "h"))
(defun get-objc-reader-impl (generator)
  (objc-make-filename generator "reader" "m"))

(defgeneric objc-field-type (generator field field-type))
(defgeneric objc-field-initform (generator field field-type))

(defun objc-local-name (generator name)
  (declare (ignore generator))
  (map 'string #'(lambda (char)
                   (if (member char '(#\Space #\Tab))
                       #\_
                       char))
       name))

(defun objc-name (generator name)
  (with-slots (prefix) generator
    (objc-local-name generator (concatenate 'string prefix name))))

(defun objc-determine-field-type (generator field)
  (declare (ignore generator))
  (with-slots (type nested-type) field
    (cond
      ((and (slot-boundp field 'nested-type)
            (= (length nested-type) 1)) :|pg-array|)
      ((slot-boundp field 'type) (intern type :keyword))
      (t :|string|))))


;;; =====================================================
;;; =====================================================

(defmethod objc-field-type ((generator objc-generator) field field-type)
  (declare (ignore generator field field-type))
  "")

(defmethod objc-field-initform ((generator objc-generator) field field-type)
  (declare (ignore generator field-type))
  (if (slot-boundp field 'default)
      (format nil " :initform ~S" (slot-value field 'default))
      ""))
              
;;; =====================================================
;;; =====================================================

(defmethod objc-field-type ((generator objc-generator)
                            field
                            (field-type (eql :|string|)))
  (declare (ignore generator field field-type))
  " :type string")

(defmethod objc-field-type ((generator objc-generator)
                            field
                            (field-type (eql :|boolean|)))
  (declare (ignore generator field field-type))
  " :type t")

(defmethod objc-field-initform ((generator objc-generator)
                            field
                            (field-type (eql :|boolean|)))
  (declare (ignore generator field-type))
  (if (and (slot-boundp field 'default)
           (member (slot-value field 'default)
                   '("yes" "t" "true" "1")
                   :test #'equal))
      (format nil " :initform t")
      (format nil " :initform nil")))

(defmethod objc-field-type ((generator objc-generator)
                            field
                            (field-type (eql :|integer|)))
  (declare (ignore generator field field-type))
  " :type integer")

(defmethod objc-field-initform ((generator objc-generator)
                            field
                            (field-type (eql :|integer|)))
  (declare (ignore generator field-type))
  (if (slot-boundp field 'default)
      (format nil " :initform ~S" (parse-integer (slot-value field 'default)))
      ""))

(defmethod objc-field-type ((generator objc-generator)
                            field
                            (field-type (eql :|pg-array|)))
  (declare (ignore generator field field-type))
  " :type list")

(defmethod objc-field-initform ((generator objc-generator)
                                field
                                (field-type (eql :|pg-array|)))
  (declare (ignore generator field-type))
  " :initform nil")

(defmethod objc-field-initform ((generator objc-generator)
                                field
                                (field-type (eql :|pg-struct|)))
  (declare (ignore generator field field-type))
  "")

;;; =====================================================
;;; =====================================================

(defun generate-defpackage (generator info)
  (with-slots (types-package reader-package) generator
    (format t "(defpackage :~A~%" types-package)
    (format t "  (:use :common-objc)~%")
    (format t "  (:export")
    (with-slots (parsed-types) info
      (let ((indent " "))
        (mapc #'(lambda (struct)
                  (format t indent)
                  (with-slots (name struct-fields) struct
                    (format t "#:~A" (objc-name generator name))
                    (mapc #'(lambda (field)
                              (with-slots (name) field
                                (format t "~%             #:~A"
                                        (objc-local-name generator name))))
                          struct-fields))
                  (setf indent "~%           "))
            parsed-types)
	(when (equalp types-package reader-package)
	  (format t indent)
	  (format t "#:parse"))))
    (format t "))~%~%")
    (format t "(in-package :~A)~%" types-package)))

;;; =====================================================
;;; =====================================================

(defgeneric objc-field-declaration (generator field field-type))

(defmethod objc-field-declaration ((generator objc-generator)
				   field
				   (field-type (eql :|string|)))
  (with-slots (name) field
    (format t "  NSString* ~A;~%" (objc-local-name generator name))))

(defmethod objc-field-declaration ((generator objc-generator)
				   field
				   (field-type (eql :|integer|)))
  (with-slots (name) field
    (format t "  int ~A;~%" (objc-local-name generator name))))

(defmethod objc-field-declaration ((generator objc-generator)
				   field
				   (field-type (eql :|boolean|)))
  (with-slots (name) field
    (format t "  BOOL ~A;~%" (objc-local-name generator name))))

(defmethod objc-field-declaration ((generator objc-generator)
				   field
				   (field-type (eql :|pg-array|)))
  (with-slots (name) field
    (format t "  NSArray* ~A;~%" (objc-local-name generator name))))

;;; =====================================================
;;; =====================================================

(defgeneric objc-field-prop-decl (generator field field-type))

(defmethod objc-field-prop-decl ((generator objc-generator)
				   field
				   (field-type (eql :|string|)))
  (with-slots (name) field
    (format t "@property (nonatomic, retain) NSString* ~A;~%"
	    (objc-local-name generator name))))

(defmethod objc-field-prop-decl ((generator objc-generator)
				   field
				   (field-type (eql :|integer|)))
  (with-slots (name) field
    (format t "@property int ~A;~%" (objc-local-name generator name))))

(defmethod objc-field-prop-decl ((generator objc-generator)
				   field
				   (field-type (eql :|boolean|)))
  (with-slots (name) field
    (format t "@property BOOL ~A;~%" (objc-local-name generator name))))

(defmethod objc-field-prop-decl ((generator objc-generator)
				   field
				   (field-type (eql :|pg-array|)))
  (with-slots (name) field
    (format t "@property (nonatomic, retain) NSArray* ~A;~%"
	    (objc-local-name generator name))))

;;; =====================================================
;;; =====================================================

(defgeneric generate-data-type-predecl (generator name info))
(defmethod generate-data-type-predecl ((generator objc-generator)
				       name (info pg-struct))
  (format t "@class ~A;~%" (objc-name generator name)))


(defgeneric generate-data-type-decl (generator name info))
(defmethod generate-data-type-decl ((generator objc-generator)
				    name (info pg-struct))
  (format t "~%")
  (format t "//======================================================~%")
  (format t "// ~A struct declaration~%" (objc-name generator name))
  (format t "//======================================================~%")
  (format t "@interface ~A : " (objc-name generator name))
  (if (slot-boundp info 'extends)
      (format t "~A" (objc-name generator (slot-value info 'extends)))
      (format t "NSObject"))
  (format t " {~%")
  (with-slots (struct-fields) info
    (mapc #'(lambda (field)
	      (objc-field-declaration generator
				      field
				      (objc-determine-field-type generator
								 field)))
	  struct-fields)
    (format t "};~%~%")
    (mapc #'(lambda (field)
	      (objc-field-prop-decl generator
				    field
				    (objc-determine-field-type generator
							       field)))
	  struct-fields))
  (format t "@end~%"))

(defgeneric generate-data-type-prop (generator name info))
(defmethod generate-data-type-prop ((generator objc-generator)
				    name (info pg-struct))
  (format t "~%")
  (with-slots (struct-fields) info
    (mapc #'(lambda (field)
	      (objc-field-prop-decl generator
				    field
				    (objc-determine-field-type generator
							       field)))
	  struct-fields))
  (format t "};~%"))

;;; ======================================================

(defgeneric objc-field-prop-init (generator field field-type))

(defmethod objc-field-prop-init ((generator objc-generator)
				 field
				 (field-type string))
  (objc-field-prop-init generator field (intern field-type :keyword)))

(defmethod objc-field-prop-init ((generator objc-generator)
				   field
				   (field-type (eql :|string|)))
  (declare (ignore field-type))
  (with-slots (name default) field
    (let ((name (objc-local-name generator name)))
      (if (and (slot-boundp field 'default)
	       (plusp (length default)))
	  (format t "~%        ~A = [[NSString alloc] initWithString:@\"~A\"];"
		    name default)
	  (format t "~%        ~A = [[NSString alloc] init];" name)))))

(defmethod objc-field-prop-init ((generator objc-generator)
				   field
				   (field-type (eql :|integer|)))
  (declare (ignore field-type))
  (with-slots (name default) field
    (let ((name (objc-local-name generator name)))
      (format t "~%        ~A = ~A;" name
	                             (if (slot-boundp field 'default)
					 default
					 0)))))

(defmethod objc-field-prop-init ((generator objc-generator)
				   field
				   (field-type (eql :|boolean|)))
  (declare (ignore field-type))
  (with-slots (name default) field
    (let ((name (objc-local-name generator name)))
      (format t "~%        ~A = ~A;"
	      name
	      (if (and (slot-boundp field 'default)
		       (member (string-downcase default)
			       '("yes" "t" "true" "1")
			       :test #'equalp))
		  "YES"
		  "NO")))))

(defmethod objc-field-prop-init ((generator objc-generator)
				   field
				   (field-type pg-array))
  (declare (ignore field-type))
  (with-slots (name) field
    (let ((name (objc-local-name generator name)))
      (format t "~%        ~A = [[NSArray alloc] init];" name))))

;;; ======================================================

(defgeneric objc-field-prop-dealloc (generator field field-type))

(defmethod objc-field-prop-dealloc (generator field field-type)
  (declare (ignore generator field field-type)))

(defmethod objc-field-prop-dealloc ((generator objc-generator)
				    field
				    (field-type string))
  (objc-field-prop-dealloc generator field (intern field-type :keyword)))

(defmethod objc-field-prop-dealloc ((generator objc-generator)
				    field
				    (field-type (eql :|string|)))
  (declare (ignore field-type))
  (with-slots (name) field
    (let ((name (objc-local-name generator name)))
      (format t "~%    [~A release];" name))))

(defmethod objc-field-prop-dealloc ((generator objc-generator)
				    field
				    (field-type pg-array))
  (declare (ignore field-type))
  (with-slots (name) field
    (let ((name (objc-local-name generator name)))
      (format t "~%    [~A release];" name))))

;;; =====================================================
;;; =====================================================

(defgeneric generate-data-type-impl (generator name info))
(defmethod generate-data-type-impl ((generator objc-generator)
				    name (info pg-struct))
  (format t "~%")
  (format t "//======================================================~%")
  (format t "// ~A struct implementation~%" (objc-name generator name))
  (format t "//======================================================~%")
  (format t "@implementation ~A~%" (objc-name generator name))
  (with-slots (struct-fields) info
    (mapc #'(lambda (field)
	      (with-slots (name) field
		(format t "~%@synthesize ~A;" (objc-local-name generator
							       name))))
	  struct-fields))
  (format t "~%~%")
  (format t "- (id)init {~%")
  (format t "    if ( ( self = [super init] ) != nil ) {")
  (with-slots (struct-fields) info
    (mapc #'(lambda (field)
	      (with-slots (type nested-type) field
		(objc-field-prop-init generator
				      field
				      (or (first nested-type) type))))
	  struct-fields))
  (format t "~%")
  (format t "    }~%")
  (format t "    return self;~%")
  (format t "}~%")
  (format t "~%")
  (format t "- (void)dealloc {")
  (with-slots (struct-fields) info
    (mapc #'(lambda (field)
	      (with-slots (type nested-type) field
		(objc-field-prop-dealloc generator
					 field
					 (or (first nested-type) type))))
	  (reverse struct-fields)))
  (format t "~%")
  (format t "    [super dealloc];~%")
  (format t "}~%")
  (format t "@end~%"))

;;; ========================================================

;;; ========================================================

(defun generate-cxml-boilerplate (generator)
  (with-slots (types-package reader-package) generator
    (unless (equalp types-package reader-package)
      (format t "(defpackage :~A~%" reader-package)
      (format t "  (:use :~A :common-objc)~%" types-package)
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
                (objc-name generator root))))
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
                                         (objc-local-name generator name)))
               (:|boolean|
                   (format nil "(setf ~A (member (string-downcase value)~%                                              '(\"yes\" \"t\" \"true\" \"1\")~%                                              :test #'equalp))"
                                         (objc-local-name generator name)))
               (t  (format nil "(setf ~A value)"
                                         (objc-local-name generator name)))))
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
(defmethod generate-data-case-line ((generator objc-generator)
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
    (format t " (item ~A) path value)~%" (objc-name generator name))
    (format t "  (with-slots (~A) item~%"
              (let ((indent ""))
                (apply #'concatenate 'string
                       (mapcar #'(lambda (field)
                                   (prog1
                                       (with-slots (name) field
                                         (format nil "~A~A"
                                                     indent
                                                     (objc-local-name
                                                        generator name)))
                                     (setf indent " ")))
                               simple-types))))
    (format t "    (case path")
    (mapc #'(lambda (field)
              (generate-data-case-line generator info field))
          simple-types)
    (format t ")))~%~%")))

(defgeneric generate-start-handler (generator field type))

(defmethod generate-start-handler ((generator objc-generator)
                                   field
                                   (type string))
  (with-slots (from type) field
    (format t "~%")
    (format t "      (:|~A|~%" from)
    (format t "             (push nil paths)~%")
    (format t "             (push (make-instance '~A) items))"
            (objc-name generator type))))

(defmethod generate-start-handler ((generator objc-generator) field
                                   (type pg-array))
  (declare (ignore field))
  (with-slots (element-types) type
    (mapc #'(lambda (element)
              (with-slots (type) element
                (generate-start-handler generator element type)))
          element-types)))

(defgeneric generate-end-handler (generator field field-type element-type))

(defmethod generate-end-handler ((generator objc-generator)
                                   field
                                   (field-type pg-array)
                                   (element-type pg-array-element))
  (declare (ignore field-type))
  (with-slots (name) field
    (with-slots (from type) element-type
      (let ((name (objc-local-name generator name)))
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

(defmethod generate-end-handler ((generator objc-generator) field
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
    (format t " (item ~A) path)~%" (objc-name generator name))
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
    (format t " (item ~A) path)~%" (objc-name generator name))
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
(defmethod generate-data-type-parser ((generator objc-generator)
                                      name (info pg-struct))
  (format t ";;; =================================================================~%" )
  (format t ";;; ~A struct~%" (objc-name generator name))
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

(defun objc-generator (generator-info output-directory &optional args)
  (let* ((option-list '(("prefix" :optional)))
         (options (nth-value 1 (getopt:getopt args option-list)))
         (prefix (or (cdr (assoc "prefix" options :test #'equal))
                     "")))
    (let ((generator (make-instance 'objc-generator
                                    :generator-info generator-info
                                    :output-directory output-directory
                                    :prefix prefix)))                         
      (let ((objc-types-header-filename (get-objc-types-header generator))
	    (objc-types-impl-filename   (get-objc-types-impl generator))
            (objc-reader-header-filename (get-objc-reader-header generator))
	    (objc-reader-impl-filename   (get-objc-reader-impl generator)))
        (with-open-file (*standard-output* objc-types-header-filename
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)

	  (format t "#import <Foundation/Foundation.h>~%")
	  (format t "~%")

          (with-slots (parsed-types) generator-info
            (mapc #'(lambda (info)
                      (generate-data-type-predecl generator
						  (slot-value info 'name)
						  info))
                  parsed-types)
            (mapc #'(lambda (info)
                      (generate-data-type-decl generator
					       (slot-value info 'name)
					       info))
                  parsed-types)))
	(with-open-file (*standard-output* objc-types-impl-filename
					   :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	  (format t "#import \"types.h\"~%")
	  (format t "~%")
          (with-slots (parsed-types) generator-info
            (mapc #'(lambda (info)
                      (generate-data-type-impl generator
					       (slot-value info 'name)
					       info))
                  parsed-types)))

#|
        (with-open-file (*standard-output* objc-reader-filename
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
|#
        (list objc-types-header-filename
	      objc-types-impl-filename
	      objc-reader-header-filename
	      objc-reader-impl-filename)))))