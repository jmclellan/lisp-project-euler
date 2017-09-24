(in-package :euler)

(defun slow-prime-p (val)
  (loop for i from 2 to (sqrt val)
        when (integerp (/ val i))
          return (values nil (/ val i))
        finally (return t)))


(defun slow-prime-factorization (val)
  (labels ((prime-fac-iter (val lst)
             (multiple-value-bind (primep divisor)
                 (slow-prime-p val)
               (if primep
                   (cons val lst)
                   (prime-fac-iter (/ val divisor)
                                   (append (prime-fac-iter divisor nil) lst))))))
    (prime-fac-iter val nil)))



(defun distinct-prime-factors (val)
  (remove-duplicates (slow-prime-factorization val)
                     :test #'=))

(defun nth-prime (number)
  (labels ((nth-prime-iter (n-goal generator)
             (multiple-value-bind (nth-prime n)
                 (funcall generator)
               (if (= n n-goal)
                   nth-prime
                   (nth-prime-iter number generator)))))
    (nth-prime-iter number (create-prime-generator))))


(defun collect-primes-under (ceiling)
  (loop with prime-generator = (create-prime-generator)
        as next-prime = (funcall prime-generator)
        while (< next-prime ceiling)
        collecting next-prime))

(defun sum-primes-under (ceiling)
  (reduce #'+ (collect-primes-under ceiling)))


(defun prime-sive (end-range)
  (labels ((sive-itter (remaining-values prime-vals)
             ;;   (format t "~&remaining-lst length: ~7d|primes-collected: ~4d"
             ;;           (length remaining-values)
             ;;           (length prime-vals))
             (if remaining-values 
                 (sive-itter (remove-if (divisible-by (car remaining-values))
                                        (cdr remaining-values))
                             (cons (car remaining-values) prime-vals))
                 prime-vals)))
    (sive-itter (cons 2 (range 3 end-range 2)) nil)))
;;; not sure why its so much slower than the original approach - perhaps it
;;; only reaps benefits after a while
