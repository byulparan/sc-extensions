(in-package :sc-extensions)

(defmacro define-code (name args &body body)
  (sc::convert-code
   `(defun ,name ,args
      ,@body)))
