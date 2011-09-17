(in-package :asdf)

(defsystem dic
  :name "dic"
  :author "Takeru Ohta"
  :version "0.0.2"

  :depends-on (:dawg :creole)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "dic")))
