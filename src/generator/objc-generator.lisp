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
   (types-file :initarg :types-file :initform "types")
   (reader-class :initarg :reader-class :initform "reader")))

(defun objc-make-filename (generator base type)
  (with-slots (output-directory) generator
    (merge-pathnames (make-pathname :name base :type type) output-directory)))

(defun get-objc-types-header (generator)
  (with-slots (types-file) generator
    (objc-make-filename generator types-file "h")))
(defun get-objc-types-impl (generator)
  (with-slots (types-file) generator
    (objc-make-filename generator types-file "m")))
(defun get-objc-reader-header (generator)
  (with-slots (reader-class) generator
    (objc-make-filename generator reader-class "h")))
(defun get-objc-reader-impl (generator)
  (with-slots (reader-class) generator
    (objc-make-filename generator reader-class "m")))

(defgeneric objc-field-type (generator field field-type))
(defgeneric objc-field-initform (generator field field-type))

(defun string-uncapitalize (string)
  (concatenate 'string (string-downcase (subseq string 0 1))
	               (subseq string 1)))

(defun objc-local-name (generator name)
  (declare (ignore generator))
  (string-uncapitalize (remove-if #'(lambda (char)
				      (member char '(#\Space #\Tab)
					      :test #'eql))
				  (string-capitalize name))))

(defun objc-name (generator name)
  (with-slots (prefix) generator
    (remove-if #'(lambda (char)
		   (member char '(#\Space #\Tab) :test #'eql))
	       (concatenate 'string (string-capitalize prefix)
			            (string-capitalize name)))))

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
    (format t "  NSMutableArray* ~A;~%" (objc-local-name generator name))))

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
    (format t "@property (nonatomic, retain) NSMutableArray* ~A;~%"
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
      (format t "~%        ~A = [[NSMutableArray alloc] init];" name))))

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
  (format t "~%")
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

(defun generate-nsxmlparser-header-boilerplate (generator)
  (with-slots (reader-class) generator
    (let ((fields '("NSString* rootPath"
		    "NSMutableArray* items"
		    "NSMutableArray* buffers"
		    "NSMutableArray* paths"
		    "NSObject* lastItem")))
      (format t "#import <Foundation/Foundation.h>~%")
      (format t "~%")
      (format t "@interface ~A : NSObject <NSXMLParserDelegate> {~%"
	        reader-class)
      (mapc #'(lambda (field)
		(format t "  ~A;~%" field))
	    fields)
      (format t "}~%")
      (format t "~%")
      (mapc #'(lambda (field)
		(format t "@property (nonatomic, retain) ~A;~%" field))
	    fields)
      (format t "~%")
      (format t "- (void)startObject:(NSObject*)_obj;~%")
      (format t "- (NSObject*)endObject;~%")
      (format t "~%")

      (format t "+ (id)parseFromURL:(NSURL*)_url;~%")
      (format t "+ (id)parseFromURL:(NSURL*)_url withRootPath:(NSString*)_rootPath;~%")
      (format t "+ (id)parseFromData:(NSData*)_data;~%")
      (format t "+ (id)parseFromData:(NSData*)_data withRootPath:(NSString*)_rootPath;~%")
      (format t "~%")
      (format t "@end~%"))))

(defun generate-nsxmlparser-impl-boilerplate (generator)
  (with-slots (types-file reader-class generator-info) generator
    (with-slots (root from parsed-types) generator-info
      (let ((fields '("rootPath" "items" "buffers" "paths" "lastItem")))
	(format t "#import \"~A.h\"~%" types-file)
	(format t "#import \"~A.h\"~%" reader-class)
	(format t "~%")

	(mapc #'(lambda (type)
		  (with-slots (name) type
		    (generate-data-type-parser generator name type)))
	      parsed-types)

	(format t "@implementation ~A~%" reader-class)
	(format t "~%")
	(mapc #'(lambda (field)
		  (format t "@synthesize ~A;~%" field))
	      fields)
	(format t "~%")

	(format t "- (id)init {~%")
	(format t "    if ( ( self = [super init] ) != nil ) {~%")
	(format t "        rootPath = @~S;~%" from)
	(format t "        items = [[NSMutableArray alloc] init];~%")
	(format t "        buffers = [[NSMutableArray alloc] init];~%")
	(format t "        paths = [[NSMutableArray alloc] init];~%")
	(format t "        lastItem = nil;~%")
	(format t "    }~%")
	(format t "    return self;~%")
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)reader {~%")
	(format t "    return [[[~A alloc] init] autorelease];~%" reader-class)
	(format t "}~%")
	(format t "~%")

	(format t "- (void)dealloc {~%")
	(mapc #'(lambda (field)
		  (format t "    [~A release];~%" field))
	      (reverse fields))
	(format t "    [super dealloc];~%")
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)parse:(NSXMLParser*)_parser withRootPath:(NSString*)_rootPath {~%")
	(format t "    ~A* reader = [~:*~A reader];~%" reader-class)
	(format t "~%")
	(format t "    if ( _rootPath != nil ) {~%")
	(format t "        reader.rootPath = _rootPath;~%")
	(format t "    }~%")
	(format t "~%")
	(format t "    [_parser setDelegate:reader];~%")
	(format t "~%")
	(format t "    if ( [_parser parse] ) {~%")
	(format t "        return reader.lastItem;~%")
	(format t "    }~%")
	(format t "    else {~%")
	(format t "        return nil;~%")
	(format t "    }~%")
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)parseFromURL:(NSURL*)_url {~%")
	(format t "    NSXMLParser* parser = [[NSXMLParser alloc] initWithContentsOfURL:_url];~%")
	(format t "    return [~A parse:[parser autorelease] withRootPath:nil];~%"
		  reader-class)
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)parseFromURL:(NSURL*)_url withRootPath:(NSString*)_rootPath {~%")
	(format t "    NSXMLParser* parser = [[NSXMLParser alloc] initWithContentsOfURL:_url];~%")
	(format t "    return [~A parse:[parser autorelease] withRootPath:_rootPath];~%"
		  reader-class)
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)parseFromData:(NSData*)_data {~%")
	(format t "    NSXMLParser* parser = [[NSXMLParser alloc] initWithData:_data];~%")
	(format t "    return [~A parse:[parser autorelease] withRootPath:nil];~%"
		  reader-class)
	(format t "}~%")
	(format t "~%")

	(format t "+ (id)parseFromData:(NSData*)_data withRootPath:(NSString*)_rootPath {~%")
	(format t "    NSXMLParser* parser = [[NSXMLParser alloc] initWithData:_data];~%")
	(format t "    return [~A parse:[parser autorelease] withRootPath:_rootPath];~%"
		  reader-class)
	(format t "}~%")
	(format t "~%")

	(format t "- (NSString*)addToPath:(NSString*)_component withSeparator:(NSString*)_separator {~%")
	(format t "    NSString* cur = (NSString*)[paths lastObject];~%")
	(format t "    if ( cur == nil ) { cur = @\"\"; }~%")
	(format t "    NSString* newPart = [_separator stringByAppendingString:_component];~%")
	(format t "    return [cur stringByAppendingString:newPart];~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)pushPath:(NSString*)_component withSeparator:(NSString*)_separator {~%")
	(format t "    [paths addObject:[self addToPath:_component withSeparator:_separator]];~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (id)getCurrentObject {~%")
	(format t "    id object = [items lastObject];~%")
	(format t "    return object;~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (id)getCurrentObjectParent {~%")
	(format t "    id object = nil;~%")
	(format t "    if ( [items count] > 1 ) {~%")
	(format t "        object = [items objectAtIndex:([items count]-2)];~%")
	(format t "    }~%")
	(format t "    return object;~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)startObject:(NSObject*)_obj {~%")
	(format t "    [paths addObject:@\"\"];~%")
	(format t "    [items addObject:_obj];~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (NSObject*)endObject {~%")
	(format t "    NSObject* object = (NSObject*)[items lastObject];~%")
	(format t "    [paths removeLastObject];~%")
	(format t "    [items removeLastObject];~%")
	(format t "    return object;~%")
	(format t "}~%")

	(format t "- (void)start:(NSString*)_path {~%")
	(format t "    id object = [self getCurrentObject];~%")
	(format t "    if ( object && [object respondsToSelector:@selector(start:path:)] ) {~%")
	(format t "        [object start:self path:_path];~%")
	(format t "    }~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)end:(NSString*)_path {~%")
	(format t "    id object = [self getCurrentObjectParent];~%")
	(format t "    if ( object && [object respondsToSelector:@selector(end:path:)] ) {~%")
	(format t "        [object end:self path:_path];~%")
	(format t "    }~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)data:(NSString*)_path value:(NSString*)_value {~%")
	(format t "    id object = [self getCurrentObject];~%")
	(format t "    if ( object && [object respondsToSelector:@selector(data:path:value:)] ) {~%")
	(format t "        [object data:self path:_path value:_value];~%")
	(format t "    }~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)parser:(NSXMLParser*)_parser didStartElement:(NSString*)_name~%")
	(format t "        namespaceURI:(NSString*)_namespaceURI qualifiedName:(NSString*)_qname~%")
	(format t "        attributes:(NSDictionary*)_attributes {~%")
	(format t "    [self pushPath:_name withSeparator:@\"/\"];~%")
	(format t "    if ( [items count] == 0~%")
	(format t "    && [rootPath compare:(NSString*)[paths lastObject]] == NSOrderedSame ) {~%")
	(format t "        [self startObject:[[[~A alloc] init] autorelease]];~%" (objc-name generator root))
	(format t "    }~%")
	(format t "    [buffers addObject:@\"\"];~%")
	(format t "    [self start:(NSString*)[paths lastObject]];~%")
	(format t "~%")
	(format t "    for ( NSString* attrib in [_attributes keyEnumerator] ) {~%")
	(format t "        [self data:[self addToPath:attrib withSeparator:@\"@\"]~%")
	(format t "             value:(NSString*)[_attributes objectForKey:attrib]];~%")
	(format t "    }~%")
	(format t "}~%")
	(format t "~%")

	(format t "- (void)parser:(NSXMLParser*)_parser didEndElement:(NSString*)_name~%")
	(format t "        namespaceURI:(NSString*)_namespaceURI~%")
	(format t "       qualifiedName:(NSString*)_qname {~%")
	(format t "    NSString* text = [buffers lastObject];~%")
	(format t "    if ( [text length] > 0 ) {~%")
	(format t "        [self data:[self addToPath:@\".\" withSeparator:@\"/\"] value:text];~%")
	(format t "    }~%")
	(format t "~%")
	(format t "    [buffers removeLastObject];~%")
	(format t "    [paths removeLastObject];~%")
	(format t "    NSString* path = (NSString*)[paths lastObject];~%")
	(format t "~%")
	(format t "    if ( [rootPath isEqualToString:path] ) {~%")
	(format t "        self.lastItem = (NSObject*)[items lastObject];~%")
	(format t "    }~%")
	(format t "~%")
	(format t "    [self end:path];~%")
	(format t "}~%")
	(format t "~%")

	(format t "-(void)parser:(NSXMLParser*)_parser foundCharacters:(NSString*)_chars {~%")
	(format t "    NSString* current = (NSString*)[buffers lastObject];~%")
	(format t "    [buffers removeLastObject];~%")
	(format t "    [buffers addObject:[current stringByAppendingString:_chars]];~%")
	(format t "}~%")

	(format t "~%")
	(format t "@end~%")))))

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
                   (format nil "self.~A = [_value intValue];"
			   (objc-local-name generator name)))
               (:|boolean|
                   (format nil "self.~A = [_value boolValue];"
			   (objc-local-name generator name)))
               (t  (format nil "self.~A = _value;"
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

(defgeneric generate-data-case-line (generator info field indent))
(defmethod generate-data-case-line ((generator objc-generator)
                                    (info pg-struct)
                                    (field pg-field)
				    indent)
  (with-slots (name from type nested-type) field
    (let ((actual-type (or (first nested-type) type)))
      (format t "~%~A" indent)
      (format t "if ( [_path isEqualToString:@~S] ) {~%" from)
      (format t "        ~A~%" (generate-value-convertor generator
							 name
							 actual-type))
      (format t "    }"))))

(defgeneric generate-start-handler (generator field type indent))

(defmethod generate-start-handler ((generator objc-generator)
                                   field
                                   (type string) indent)
  (with-slots (from type) field
    (format t "~%~A" indent)
    (format t "if ( [_path isEqualToString:@~S] ) {~%" from)
    (format t "        [_parser startObject:[[[~A alloc] init] autorelease]];~%" (objc-name generator type))
    (format t "    }")))

(defmethod generate-start-handler ((generator objc-generator) field
                                   (type pg-array) indent)
  (declare (ignore field))
  (with-slots (element-types) type
    (mapc #'(lambda (element)
              (with-slots (type) element
                (generate-start-handler generator element type indent))
	      (setf indent "    else "))
          element-types)))

(defgeneric generate-end-handler (generator field
					    field-type element-type indent))

(defmethod generate-end-handler ((generator objc-generator)
                                   field
                                   (field-type pg-array)
                                   (element-type pg-array-element)
				   indent)
  (declare (ignore field-type))
  (with-slots (name) field
    (with-slots (from) element-type
      (let ((name (objc-local-name generator name)))
	(format t "~%~A" indent)
	(format t "if ( [_path isEqualToString:@~S] ) {~%" from)
	(format t "        [~A addObject:[_parser endObject]];~%" name)
	(format t "    }")))))

(defmethod generate-end-handler ((generator objc-generator) field
                                   (field-type pg-array)
                                   (element-type (eql nil))
				   indent)
  (with-slots (element-types) field-type
    (mapc #'(lambda (element)
              (generate-end-handler generator field
                                    field-type
                                    element
				    indent)
	      (setf indent "    else "))
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

  (with-slots (reader-class) generator

    (with-slots (struct-fields) info
      (let ((simples (remove-if-not #'(lambda (field)
					(is-simple generator field))
				    struct-fields))
	    (complexes (remove-if #'(lambda (field)
				      (is-simple generator field))
				  struct-fields))
	    (name (objc-name generator name)))
	(format t "@interface ~A (~A_HelperFunctions)~%" name reader-class)
	(when simples
	  (format t "- (void)data:(~A*)_parser path:(NSString*)_path value:(NSString*)_value;~%" reader-class))
	(when complexes
	  (format t "- (void)start:(~A*)_parser path:(NSString*)_path;~%" reader-class)
	  (format t "- (void)end:(~A*)_parser path:(NSString*)_path;~%" reader-class))
	(format t "@end~%")
	(format t "~%")

	(format t "@implementation ~A (~A_HelperFunctions)"
		  name reader-class)
	(when simples
	  (format t "~%")
	  (format t "- (void)data:(~A*)_parser path:(NSString*)_path value:(NSString*)_value {" reader-class)
	  (let ((indent "    "))
	    (mapc #'(lambda (field)
		      (with-slots (type nested-type from) field
			(generate-data-case-line generator
						 info
						 field
						 indent))
		      (setf indent "    else "))
		  simples))
	  (format t "~%")
	  (format t "}~%"))
	(when complexes
	  (format t "~%")
	  (format t "- (void)start:(~A*)_parser path:(NSString*)_path {" reader-class)
	  (let ((indent "    "))
	    (mapc #'(lambda (field)
		      (with-slots (type nested-type from) field
			(generate-start-handler generator
						field
						(or (first nested-type)
						    type)
						indent))
		      (setf indent "    else "))
		  complexes))
	  (format t "~%")
	  (format t "}~%")
	  (format t "~%")
	  (format t "- (void)end:(~A*)_parser path:(NSString*)_path {" reader-class)
	  (let ((indent "    "))
	    (mapc #'(lambda (field)
		      (with-slots (type nested-type from) field
			(generate-end-handler generator
					      field
					      (or (first nested-type)
						  type)
					      nil
					      indent))
		      (setf indent "    else "))
		  complexes))
	  (format t "~%")
	  (format t "}~%"))
	(format t "@end~%")
	(format t "~%")))))

(defun objc-generator (generator-info output-directory &optional args)
  (let* ((option-list '(("prefix" :optional)
			("types-file" :optional)
			("reader-class" :optional)))
         (options (nth-value 1 (getopt:getopt args option-list)))
         (prefix (or (cdr (assoc "prefix" options :test #'equal))
                     ""))
	 (types-file (or (cdr (assoc "types-file" options :test #'equalp))
			 "Types"))
         (reader-class (or (cdr (assoc "reader-class" options :test #'equal))
			   "Reader")))
    (let ((generator (make-instance 'objc-generator
                                    :generator-info generator-info
                                    :output-directory output-directory
                                    :prefix prefix
				    :types-file types-file
				    :reader-class reader-class)))
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
	  (with-slots (types-file) generator
	    (format t "#import \"~A.h\"~%" types-file))
	  (format t "~%")
          (with-slots (parsed-types) generator-info
            (mapc #'(lambda (info)
                      (generate-data-type-impl generator
					       (slot-value info 'name)
					       info))
                  parsed-types)))

        (with-open-file (*standard-output* objc-reader-header-filename
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
	  (generate-nsxmlparser-header-boilerplate generator))

        (with-open-file (*standard-output* objc-reader-impl-filename
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
	  (generate-nsxmlparser-impl-boilerplate generator))

        (list objc-types-header-filename
	      objc-types-impl-filename
	      objc-reader-header-filename
	      objc-reader-impl-filename)))))