(defpackage dic
  (:use :common-lisp)
  (:shadow :common-lisp load)
  (:export load
           lookup
           make-command-and-die))
(in-package :dic)

