(asdf:defsystem #:com.nklein.parser-generator.types
  :serial t
  :components ((:module "src"
		:components ((:module "parser"
			      :components ((:file "types")))))))
