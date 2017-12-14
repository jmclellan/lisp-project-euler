(in-package :euler)

(defun euler-problem-52 (multiples)
  (loop for i from 2
      as multiple-list = (mapcar (lambda (multiple)
                                   (* i multiple))
                                 multiples)
      when (apply #'numerical-permutations-p multiple-list)
        return multiple-list))

(defun euler-52 ()
  (euler-problem-52 (range 1 6)))

(defun n-choose-r (n r)
  (/ (factorial n)
     (* (factorial r) (factorial (- n r)))))

(defun euler-53 ()
  (loop for n from 1 to 50
      with total-count = 0
      do (loop for r from 1 to n
               when (> (n-choose-r n r) (expt 10 6))
                 do (incf total-count))
         finally (return total-count)))

;;; euler-54
;; (with-open-file (in "resources/p054_poker.txt" 
;;                     :direction :input)
;;   (defvar test-cards (cl-ppcre:split " " (read-line in))))
;; (defvar hand1 (subseq test-cards 0 5))
;; (defvar hand2 (subseq test-cards 5))

;; (defun royal-flush-p (hand)
;;   ""
;;   (char= (mapcar (lambda (card)
;;                    (aref card 1))
;;                  hand)
;;   )

(defclass card ()
  ((value :reader card-value :initarg :card-value)
   (suit :reader suit :initarg :suit)
   )
    )


(defun reverse-number (val)
  (parse-integer (reverse (write-to-string val))))

(defun lychrel-number-p (val)
  (loop for iteration from 1 to 50
        with current-number = val
        do (setf current-number (+ current-number (reverse-number current-number)))
        when (palindrome-p current-number)
          return (values nil current-number iteration)
        finally (return t)))

(defun euler-55 ()
  (count-if #'lychrel-number-p (range 1 (expt 10 4))))


(defun euler-56 ()
  (loop for a below 100
        with max-digit-sum = 0
        do (loop for b below 100
                 as cur-value = (expt a b)
                 as cur-digit-sum = (digit-sum cur-value)
                 when (> cur-digit-sum max-digit-sum)
                   do (setf max-digit-sum cur-digit-sum))
        finally (return max-digit-sum)))

;; (defun euler-toitent-function (val)
;;   (reduce #'* (slow-prime-factorization val) :key #'1-))

;; 58 is going to be using the #'number-spiral-closure that was made earlier for another function
;; (defun euler-58 ()
;;   (loop for side-length from 1 by 2
;;         with corner-generator = (number-spiral-closure)
;;         with total = 1
;;         with primes = 0
;;         do (loop repeat 4 ;; this loop adds one more layer to our count
;;                    initially (incf total 4) ;; there will always be 4 corners
;;                  as next-corner = (funcall corner-generator) ; gen actual values 
;;                  when (slow-prime-p next-corner) ; check for primality & handle accordingly
;;                    do (incf primes))
;;         when (<= (/ primes total) 1/10)
;;           return side-length))

(defun cube-p (val)
  (multiple-value-bind (integer remainder)
      (truncate (expt val 1/3))
    (declare (ignore integer))
    (zerop remainder)))

;; loop for n
;; find the n^3
;; find all lexical permutations of n^3
;; count how many permutations are cube values

;; (loop
;;   with reference-key = nil
;;   for n from 1 to 1000
;;   as n-cubed = (expt n 3)
;;   as cubed-digit-str = (sort (write-to-string n-cubed) #'char>)
;;   as record = (list :digits cubed-digit-str :val n :count 1)
;;   do (pprint reference-key)
;;      do (pprint record)
;;   if (find cubed-digit-str reference-key
;;            :key (lambda (plst)
;;                   (getf plst :digits))
;;            :test #'string=)
;;     do (format t "~&the record does indeed exist")
;;        (incf (getf (find cubed-digit-str reference-key
;;                          :key (lambda (plst)
;;                                 (getf plst :digits))
;;                          :test #'string=) :count))
;;   else do 
;;     (format t "the record does not exist")
;;     (push record reference-key)
;;   finally (return reference-key))




;;; euler 63
#|
=> 10^(n-1) <= x^n < 10^(n)
=> (n-1)*log(10) <= n*log(x) < (n)*log(10)
-- log(10) using base 10 is 1 --
=> (n-1) <= n*log(x) < (1)
=> (n-1)/n <= log(x) < (n)/n
=> 10^(n-1)/n) <= x < 10^(n)/n)
=> 10^(n-1)/n) <= x < 10
we want to iterate through all n until 10 - 10^(n-1)/n) is less than 1
|#
(defun euler-63 ()
  (loop for n from 1
      as lower-bound = (expt 10 (/ (1- n) n))
      ;; the upper bound is 10 based on the rewrite of the formula
      until (<= (- 10 lower-bound) 1)
      as integer-distance = (truncate  (- 10 lower-bound) 1)
      unless (zerop integer-distance)
        summing integer-distance))


;; this uses the same code as 18 
(defun euler-67 ()
  (pyramid-reduction (read-pyramid-data "resources/p067_triangle.txt")))


(defun euler-toitent-function (val)
  "using eulers product formula"
  (reduce #'* (distinct-prime-factors val)
          :key (lambda (prime)
                 (- 1  (/ 1 prime)))
          :initial-value val))

(defun euler-problem-69 (ceiling)
  (loop for i from 2 to ceiling
        with max-i = 0
        with ratio = 0
        as toitent = (euler-toitent-function i)
        as cur-ratio = (/ i toitent)
        when (> cur-ratio ratio)
          do (setf max-i i
                   ratio cur-ratio)
        finally (return max-i)))

(defun euler-69 ()
  (euler-problem-69 (expt 10 6)))

(defun permutation-p (str1 str2)
  (string= (sort str1 #'char<=) (sort str2 #'char<=)))

(defun numerical-permutation-p (val1 val2)
  (permutation-p (write-to-string val1) (write-to-string val2)))

(defun numerical-permutations-p (&rest numeric-args)
  (let ((sorted-numeric-strings (mapcar #'(lambda (val)
                                            (sort (write-to-string val) #'char<=))
                                        numeric-args)))
    (every #'(lambda (str)
               (string= (car sorted-numeric-strings) str))
           (cdr sorted-numeric-strings))))

;; (defun euler-problem-70 (ceiling-value)
;;   (loop for n from 2 to (1- ceiling-value)
;;         with n.ratio = (cons 0 0)
;;         as toitent-val = (euler-toitent-function n)
;;         as cur-ratio = (/ n toitent-val)
;;         when (and (numerical-permutation-p n toitent-val)
;;                   (< cur-ratio (cdr n.ratio)))
;;           do (setf n.ratio (cons n cur-ratio))
;;         finally (return (car n.ratio))))

(defun euler-problem-71 (max-denominator target-fraction)
  (loop for denom from 1 to max-denominator
      with goal-fraction = target-fraction
      with left-most-fraction = 0
      do (loop for numerator from (floor (* left-most-fraction denom))
               as cur-fraction = (/ numerator denom)
               when (>= cur-fraction goal-fraction)
                 return nil
               when (> cur-fraction left-most-fraction)
                 do (setf left-most-fraction cur-fraction))
        finally (return left-most-fraction)))

(defun euler-71 ()
  (numerator (euler-problem-71 (expt 10 6) 3/7)))

(defun euler-problem-72 (ceiling-value)
  (loop for denominator from 2 to ceiling-value
       summing (euler-toitent-function denominator)))

(defun euler-72 ()
  (euler-problem-72 (expt 10 6)))

(defun euler-problem-73 (max-denominator lower-fence upper-fence)
  (loop for denom from 1 to max-denominator
        with fraction-count = 0
        do (loop for numerator from (floor (* lower-fence denom))
                 as cur-fraction = (/ numerator denom)
                 when (>= cur-fraction upper-fence)
                   return nil
                 when (and
                       (= (gcd numerator denom) 1)
                       (> cur-fraction lower-fence))
                   do (incf fraction-count))
        finally
           (return fraction-count)))

(defun euler-73 ()
  (euler-problem-73 12000 1/3 1/2))

(defun digit-factorial (val)
  (reduce #'+ (write-to-string val)
          :key (lambda (digit-char)
                 (factorial (digit-char-p digit-char)))))


(defun digit-factorial-chain (val)
  (loop
    with cur-val = (digit-factorial val)
    if (or (= cur-val val) (member cur-val chain :test #'=))
      return (values (1+ chain-length) (cons val chain) cur-val)
    else collect cur-val into chain
         and count 1 into chain-length
         and do (setf cur-val (digit-factorial cur-val))))

(defun euler-problem-74 (ceiling-value)
  (time (loop for i from 1 to ceiling-value
              as chain-length = (digit-factorial-chain i)
            when (= chain-length 60)
              count 1 into long-chain-count
              finally (return long-chain-count))))

(defun euler-74 ()
  (euler-problem-74 (expt 10 6)))

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
  


;;92
(defun square-digit-sum (val)
  (reduce #'+ (write-to-string val) :key (lambda (char) (expt (digit-char-p char) 2))))

(defun square-digit-chain (val &optional existing-chain)
  (if (or (= val 89) (= val 1))
      (values val existing-chain) ; the order of this chain is reveresed
      (square-digit-chain (square-digit-sum val) (cons val existing-chain))))

(defun euler-92 ()
  (loop for i from 1 to (1- (expt 10 7)) ; 10 million
        as cur-chain-result = (square-digit-chain i)
        when (= cur-chain-result 89)
          count 1))

;; (defun memoized-square-digit-chain (cache-length)
;;   (let ((cache (make-array cache-length :initial-element nil))) 
;;     (labels ((square-digit-chain (val &optional existing-chain)
;;                                  (cond ((and (< val (1- cache-length)) ; fits in our cache
;;                                              (aref cache val)) ; non-nil value
;;                                         ;; we have a value and we know where it ends up
;;                                         ;; fill out the cahce for all values
;;                                         (mapcar
;;                                          (lambda (chain-link)
;;                                            (when (< chain-link (1- cache-length))
;;                                              (setf (aref cache chain-link) (aref cache val))))
;;                                          existing-chain)
;;                                         (aref cache val)) ; then just return the value
;;                                        ((or (= val 89) (= val 1))
;;                                         (mapcar
;;                                          (lambda (chain-link)
;;                                            (when (< chain-link (1- cache-length))
;;                                              (setf (aref cache chain-link) (aref cache val))))
;;                                          existing-chain)
;;                                         (values val existing-chain))
;;                                        (t (square-digit-chain (square-digit-sum val)
;;                                                               (cons val existing-chain))))))
;;             #'square-digit-chain
      
;;       )
;;     )
;;   )

;;; memoize a large swath of values and then use that to keep track of values to speed
;;; up iterating through the all the values under ten million 


(defun euler-99 ()
  (let ((data (with-open-file (file "resources/p099_base_exp.txt" :direction :input)
                (loop
                  for counter from 1
                  as data = (read-line file nil)
                  ;;do (format t "~&line ~d data is ~a" counter data)
                  unless data
                    return data-set
                  collect (cons counter (cl-ppcre:split "," data)) into data-set))))
    (car (reduce #'(lambda (a b)
                (if (> (* (parse-integer (third a)) (log (parse-integer (second a))))
                       (* (parse-integer (third b)) (log (parse-integer (second b)))))
                    a 
                    b))
            data))))
