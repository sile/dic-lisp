(defpackage dic
  (:use :common-lisp)
  (:shadow :common-lisp load)
  (:export load
           lookup
           make-command-and-die

           dic
           entry
           entry-key
           entry-title
           entry-summary
           entry-data))
(in-package :dic)

