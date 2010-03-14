(in-package :com.nklein.parser-generator)

(defun main ()
  (lisp-generator (parse #P"examples/parser_generator_parser.xml")
		  #P"./gen/"
		  :types-package "com.nklein.parser-generator.types"))
