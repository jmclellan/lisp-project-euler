(require :asdf)
(require :quicklisp)


(asdf::defsystem euler
  :name "euler"
  :version "0.0.1"
  :author "Joshua Mclellan"
  :serial t
  :components ( (:file "prime")
                (:file "problem-data")
                (:file "sequences")
                (:file "utilities")
                (:file "1-50")
               ))

(ql:quickload :cl-ppcre)

(defpackage :euler
  (:use :common-lisp :common-lisp-user))

(asdf::load-system :euler)


(in-package :euler)
