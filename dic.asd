(in-package :asdf)

(defsystem dic
  :name "dic"
  :author "Takeru Ohta"
  :version "0.0.1"

  :depends-on (:dawg)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "dic")))
