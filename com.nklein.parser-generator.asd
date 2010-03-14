(asdf:defsystem #:com.nklein.parser-generator
  :depends-on (#:com.nklein.parser-generator.reader)
  :components ((:module "src"
		:components ((:module "generator"
			      :serial t
			      :components ((:file "package")
					   (:file "base")
					   (:file "lisp-generator")
					   (:file "validation")
					   (:file "parser-generator")))))))
