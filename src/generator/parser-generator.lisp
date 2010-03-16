(in-package :com.nklein.parser-generator)

(defun main (&optional (args sb-ext:*posix-argv*))
  (let* ((option-list '(("language" :optional)
			("output-directory" :optional)
			("file" :optional)))
	 (options (nth-value 1 (getopt:getopt args option-list)))
	 (language (or (cdr (assoc "language" options :test #'equalp))
		       "lisp"))
	 (output-directory (or (cdr (assoc "output-directory" options
					   :test #'equalp))
			       "./"))
	 (input-file (cdr (assoc "file" options :test #'equalp))))
    (let ((parsed-input (parse (if input-file
				   (parse-namestring input-file)
				   *standard-input*))))
      (cond
	((equal language "lisp")
	      (com.nklein.parser-generator.lisp-generator::lisp-generator
	           parsed-input output-directory args))
	((equal language "objc")
	      (com.nklein.parser-generator.objc-generator::objc-generator
	           parsed-input output-directory args))
	(t (error "Unknown language: ~S" language))))))
