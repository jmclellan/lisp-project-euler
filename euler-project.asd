(require :asdf)


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
  
  (format t "mehehehee")



(defpackage :euler
  (:use :common-lisp :common-lisp-user))

(asdf::load-system :euler)

;;(common-lisp-user::make-package :euler
;;                                :use '(:common-lisp-user))


(in-package :euler)
