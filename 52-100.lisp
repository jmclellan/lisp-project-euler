(in-package :euler)

;;; euler 89


(let ((numeral-values '((#\I . 1) (#\V . 5)
                        (#\X . 10) (#\L . 50)
                        (#\C . 100) (#\D . 500)
                        (#\M . 1000))))
  (defun validate-roman-numerals (numerals)
    "filter that review the numeral string, if it is invalid it errors, otherwise it returns
     a copy of the string"
    (map 'string #'(lambda (char)
                     (if (assoc char numeral-values :test #'char=)
                         char
                         (error "this is not a valid string of roman numerals")
                         ))
         numerals))

    (defun roman-numeral-reader (numerals)
  "takes a string of roman numerals and converts in to a number"
      (reduce #'+ numerals :key (let ((last-val)) ;track last-val to know when to decrement
                                  (lambda (numeral-char) ; take in char
                                    (let ((val (cdr (assoc numeral-char numeral-values 
                                                           :test #'char=)))) ;find its value
                                      (prog1 ;preform logic for return value THEN reset last-val
                                          (if (and last-val (< last-val val))
                                              (- val (* 2 last-val)) 
                                              val)
                                        ;; if we have to decrement we also have to correct for
                                        ;; the extra addition that the reduce did one character
                                        ;; earlier
                                        (setf last-val val))))))))

(defun write-to-numerals (val)
  "adds some protection to write out M numerals until the number is under 4000 and the lisp
   format statement can take care of it its self"
  (let* ((extra-val (- val 3999))
         (additional-m-numerals (if (plusp extra-val)
                                    (ceiling extra-val 1000)
                                    0)))
    (concatenate 'string
                 (make-string additional-m-numerals :initial-element #\M)
                 (format nil "~@r" (- val (* additional-m-numerals 1000))))))
(defun euler-problem-89 ()
  (with-open-file (numeral-stream "resources/p089_roman.txt" :direction :input)
    (loop as current-value = (read-line numeral-stream nil nil nil)
          while current-value
          summing (- (length current-value)
                     (length (write-to-numerals (roman-numeral-reader current-value)))))))
  
