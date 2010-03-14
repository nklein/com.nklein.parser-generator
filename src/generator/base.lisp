(in-package :com.nklein.parser-generator)

(defclass generator ()
  ((generator-info :initarg :generator-info
		   :initform (error "Must have generator info"))
   (output-directory :initarg :output-directory :initform #P".")))
