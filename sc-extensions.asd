(asdf/defsystem:defsystem #:sc-extensions
  :name "sc-extensions"
  :author "Park Sungmin. byulparan@gmail.com"
  :description "additional library collection for cl-collider"
  :licence "Public Domain / 0-clause MIT"
  :serial t
  :depends-on (#:cl-collider
	       #:alexandria
	       #:named-readtables)
  :components ((:file "package")
	       (:file "util")
	       (:file "tempo")
	       (:file "ugens")
	       (:file "pc")))
