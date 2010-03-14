(asdf:defsystem #:com.nklein.parser-generator.reader
  :depends-on (#:com.nklein.parser-generator.types #:cxml)
  :serial t
  :components ((:module "src"
		:components ((:module "parser"
			      :components ((:file "reader")))))))
