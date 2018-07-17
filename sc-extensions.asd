(asdf/defsystem:defsystem #:sc-extensions
  :serial t
  :depends-on (#:sc
	       #:alexandria
	       #:named-readtables)
  :components ((:file "package")
	       (:file "util")
	       (:file "tempo")
	       (:file "ugens")
	       (:file "pc")))
