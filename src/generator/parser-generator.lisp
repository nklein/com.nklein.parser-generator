(in-package :com.nklein.parser-generator)

(defun main (&optional (args sb-ext:*posix-argv*))
  (let ((options (nth-value 1 (getopt:getopt args
					     '(("language" :optional)
					       ("output-directory" :required)
					       ("file" :optional))))))
    (let ((language (or (cdr (assoc "language" options :test #'equalp))
			"lisp"))
	  (output-directory (or (cdr (assoc "output-directory"
					    options
					    :test #'equalp))
				#P"./"))
	  (input-file (cdr (assoc "file" options :test #'equalp))))
      (let ((parsed-input (parse (if input-file
				     (parse-namestring input-file)
				     *standard-input*))))
	(cond
	  ((equal language "lisp")
	   (lisp-generator parsed-input output-directory args)))))))
