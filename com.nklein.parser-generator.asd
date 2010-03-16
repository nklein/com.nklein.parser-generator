(asdf:defsystem #:com.nklein.parser-generator
  :depends-on (#:com.nklein.parser-generator.reader #:getopt)
  :components ((:module "src"
		:components ((:module "generator"
			      :serial t
			      :components ((:file "package")
					   (:file "lisp-generator")
					   (:file "objc-generator")
					   (:file "validation")
					   (:file "parser-generator")))))))
