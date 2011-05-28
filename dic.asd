(require :asdf)

(defsystem dic
  (:name "dic")
  (:author "Takeru Ohta")
  (:version "0.0.1")
  (:description "dictionary")

  (:depends-on (:cl-dawg))
  (:components ((:file "package")
                (:file "dic"))))
