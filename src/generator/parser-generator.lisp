(in-package :com.nklein.parser-generator)

(defun parser-generator ( input output-directory
			 &key (generator #'lisp-generator)
			      prefix)
  (funcall generator input output-directory :prefix prefix))

(defun main ()
  (parser-generator (parse #P"examples/parser_generator_parser.xml")
		    #P"./gen/"))
