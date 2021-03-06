(in-package :euler)

(defun euler-problem-1 (ceiling-value)
  (loop for i below ceiling-value
        when (or (zerop (mod i 3))
                 (zerop (mod i 5)))
          summing i))

(defun euler-1 ()
  (euler-problem-1 1000))

;(proclaim (inline 'fib-assist)) ;cant remember syntax rn but thats a decent idea

(defun euler-problem-2 (ceiling-val filter-fn reduction-fn)
  (loop
    with fib-generator = (create-fib-generator)
    as nth-fib = (funcall fib-generator)
    while (< nth-fib ceiling-val)
    when (funcall filter-fn nth-fib)
      collect nth-fib into valid-fibs
    finally (return (reduce reduction-fn valid-fibs))))

(defun euler-2 ()
  (euler-problem-2 4000000 #'evenp #'+))


(defun euler-problem-3 (value)
  (reduce #'max (slow-prime-factorization value)))

(defun euler-3 ()
  (euler-problem-3 600851475143))
  

(defun generate-unique-vals (start end) ;; terrible name. will have to fix this later
  (let ((unique-vals))
    (mapcar (lambda (x-val)
              (mapcar (lambda (y-val)
                        (push (* x-val y-val) unique-vals))
                      (range x-val end)))
            (range start end))
    unique-vals))

(defun euler-problem-4 (start end)
  (reduce #'max (remove-if-not #'palindrome-p (generate-unique-vals start end))))

(defun euler-4 ()
  (euler-problem-4 100 999))

(defun euler-problem-5 (start end)
  (apply #'lcm (range start end)))

(defun euler-5 ()
  (euler-problem-5 1 20))

(defun euler-problem-6 (start end)
  (- (expt (reduce #'+ (range start end)) 2)
     (reduce #'+ (mapcar (lambda (val) (expt val 2)) (range start end)))))

(defun euler-6 ()
  (euler-problem-6 1 100))

(defun euler-problem-7 (number)
  (nth-prime number))

(defun euler-7 ()
  (euler-problem-7 10001))


(defun largest-product-in-series (numb-digits)
  (reduce #'max
        (maplist (lambda (lst)
                   (let ((relevent-values (ignore-errors (subseq lst 0 numb-digits))))
                     (if relevent-values
                         (apply #'* relevent-values)
                         0)))
                 (remove nil (mapcar #'digit-char-p (coerce +euler-8-data+ 'list))))))

(defun euler-problem-8 (number-of-adjacent-digits)
  (largest-product-in-series number-of-adjacent-digits))

(defun euler-8 ()
  (euler-problem-8 13))

;;;; skipping 9 for a little out of personal convinence

(defun pythagorean-triplet-p (a b c)
  (= (+ (expt a 2) (expt b 2)) (expt c 2)))

(defun euler-problem-9 (upper-bound perimiter-goal)
  (loop for a from 1 to upper-bound
        with triplet = nil
        do (loop for b from a to upper-bound
                 as c = (- perimiter-goal (+ a b))
                 until (> (+ a b) (* .70 perimiter-goal))
                 when (and (= (+ a b c) perimiter-goal)
                           (pythagorean-triplet-p a b c ))
                   do (format t "~&the triplet is: ~a ~a ~a" a b c)
                      (setf triplet (list a b c)))
        when triplet
          return (values (apply #'* triplet) triplet)))

(defun euler-9 ()
  (euler-problem-9 1000 1000))

(defun euler-problem-10 (ceiling-value)
  (reduce #'+ (collect-primes-under ceiling-value)))

(defun euler-10 ()
  (euler-problem-10 (* 2 (expt 10 6))))


(defun euler-11 ()
  (flet ((find-all-combos (data-array h w)
           (values
            ;; horizontal
            (ignore-errors (* (aref data-array h  w) 
                              (aref data-array h (+ w 1))
                              (aref data-array h (+ w 2))
                              (aref data-array h (+ w 3))))
            ;; vert 
            (ignore-errors (* (aref data-array h  w)
                              (aref data-array (+ h 1) w)
                              (aref data-array (+ h 2) w)
                              (aref data-array (+ h 3) w)))
            ;; negative slope
            (ignore-errors (* (aref data-array h  w)
                              (aref data-array (+ h 1) (+ w 1))
                              (aref data-array (+ h 2) (+ w 2))
                              (aref data-array (+ h 3) (+ w 3))))
            ;; positive slopw
            (ignore-errors (* (aref data-array h  w)
                              (aref data-array (+ h 1) (- w 1))
                              (aref data-array (+ h 2) (- w 2))
                              (aref data-array (+ h 3) (- w 3)))))))
    (let* ((width 20)
           (height 20)
           (numbers-in-a-row 4)
           (data-array  (with-open-file (fs "resources/p011_number grid.txt"
                                            :direction :input)
                          (loop repeat width
                                collecting (loop repeat height
                                                 collecting (read fs))
                                  into rows
                                finally (return (make-array (list 20 20)
                                                            :initial-contents rows))))))
      (loop for w below width
            appending (loop for h below height
                            appending (multiple-value-list (find-all-combos data-array h w)))
              into all-products
            finally (return (reduce #'max (remove nil all-products)))))))
         


;; bug in above - will collect sqrt twice ie for 9 3 will appear twice





(defun euler-problem-12 (divisor-count)
 (loop for n from 1
      as cur-tri-num = (nth-triangle-number n)
      when (>= (count-divisors cur-tri-num) divisor-count)
        return cur-tri-num))

(defun euler-12 ()
  (euler-problem-12 500))

(defun euler-13 () 
  (with-open-file (fs "resources/p013_large_sums.txt"
                      :direction :input)
    (loop as new-val = (read fs nil nil)
          while new-val
          summing new-val into total
          finally (return (subseq
                           (write-to-string total)
                           0
                           10)))))

(defun euler-problem-14 (ceiling-value)
  (loop for i from 1 to ceiling-value
        collecting (cons i (collatz-length i)) into result-pairs
        finally (return (reduce (lambda (p1 p2)
                                  (if (> (cdr p1) (cdr p2))
                                      p1
                                      p2))
                                result-pairs))))

(defun euler-14 ()
  (euler-problem-14 1000000))


(defun euler-problem-15 (lattice-width)
  (central-binomial-coefficent lattice-width))

(defun euler-15 ()
  (euler-problem-15 20))

(defun euler-problem-16 (base power)
  (digit-sum (expt base power)))

(defun euler-16 ()
  (euler-problem-16 2 1000))


;; this is limited by the range of ~r directive, totally within the scope of our
;; problem though. also we need to add three when there is a 10s place
(defun number-letter-count (number)
 (+ (count-if #'alpha-char-p (format nil "~r" number))
     (if (and (>= number 100) (plusp (mod number 100)))
         3
         0))) 

(defun euler-problem-17 (ceiling-value)
  (loop for i from 1 to ceiling-value
        summing (number-letter-count i)))

(defun euler-17 ()
  (euler-problem-17 1000))

(defun pyramid-reduction (nested-data)
  "for use in problems 18 and 67, takes a seqence of seqences whiles lengths range
   from n, n-1, n-2, ... ,1 and reduces them to find the best path from bottom to top"
  (car (reduce #'(lambda (seq1 seq2)
                   (loop for index below (length seq2)
                         collecting (+ (elt seq2 index)
                                       (max (elt seq1 index)
                                            (elt seq1 (1+ index))))))
               (sort nested-data #'> :key #'length))))

(defun read-pyramid-data (file-path)
  (with-open-file (in (probe-file file-path)
                      :direction :input)
    (loop as line = (read-line in nil nil)
          while line collect (with-input-from-string (str-in line)
                               (loop as val = (read str-in nil nil)
                                     while val collect val)))))

(defun euler-problem-18 ()
  (pyramid-reduction (read-pyramid-data "resources/p018_triangle.txt")))

;;; this has no great option to reasonably do a layer ontop but we make an second to
;;; keep consistancy

(defun euler-18 ()
  (euler-problem-18))


(setq *print-circle* t)

(defun make-circular (lst)
  (setf (cdr (last lst)) lst))

(defun leap-year-p (year)
  (let ((divisible-by-4? (zerop (mod year 4)))
        (divisible-by-100? (zerop (mod year 100)))
        (divisible-by-400? (zerop (mod year 400))))
    (when (and divisible-by-4? (or (not divisible-by-100?)
                                   (and divisible-by-100?
                                        divisible-by-400?)))
      t)))

(defun days-in-month (mth-str year)
  (let ((leap-year? (leap-year-p year)))
    (cond ((member mth-str '("SEP" "APR" "JUN" "NOV") :test #'string=) 31)
          ((and (string= mth-str "FEB") leap-year?) 29)
          ((string= mth-str "FEB") 28)
          (t 30))))


 ;; (let ((days (make-circular '("MON" "TUE" "WED"
 ;;                            "THU" "FRI" "SAT"
 ;;                            "SUN")))
 ;;     (months (make-circular '("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
 ;;                              "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))))
 ;;   (loop
 ;;    ;; for i from 1 to 30
 ;;     with total-count = 0
 ;;     with year = 1900
 ;;     as month = (pop months)
 ;;     do (loop for day from 1 to (days-in-month month year)
 ;;              as daystr = (pop days)
 ;;              ;do (format t "~%~a ~d, ~d was a ~a" month day year daystr)
 ;;              when (and (= day 1)
 ;;                        (string-equal daystr "SUN")
 ;;                        (>= year 1901))
 ;;                do
 ;;                   (format t "~%~a ~d, ~d was a ~a" month day year 
 ;;                   (incf total-count))
 ;;     when (and (= year 2000) (string-equal month "DEC"))
 ;;       return total-count
 ;;     when (string-equal month "DEC")
 ;;       do (incf year)))





(defun euler-problem-20 (val)
  (digit-sum (factorial val)))

(defun euler-20 ()
  (euler-problem-20 100))


(defun euler-problem-21 (ceiling-value)
  (loop for i below ceiling-value
        when (amicable-number-p i)
          sum i))

(defun euler-21 ()
  (euler-problem-21 (expt 10 4)))

;;; euler 22
;;;; use score-string function and map it across the sorted list of names with a closure which will then cause everyting to work out
(defun euler-problem-22 (str-lst)
  (reduce #'+ (sort str-lst #'string<=) :key (let ((counter 0))
                                                       (lambda (word)
                                                         (* (score-string word)
                                                            (incf counter))))))

(defun euler-22 ()
  (with-open-file (file-stream "resources/p022_names.txt" :direction :input)
    (euler-problem-22 (mapcar #'read-from-string (cl-ppcre:split "," (read-line file-stream))))))

;;23
;;; collect all abundant numbers below the ceiling,
;;; find all sums of two
;;; have an array of n slots (where n is the ceiling)
;;; as we sum all values together we can mark array slots t
;;; at the end count and return the number of nils remaining
(defun euler-23 ()
  (let* ((ceiling-value 28123)
         (abundant-vals (loop for i from 1 to ceiling-value
                              when (abundant-number-p i)
                                collect i))

         (reference-array (make-array ceiling-value :element-type 'bit)))
    (loop while abundant-vals
          as first-term = (car abundant-vals)
          do (loop for second-term in abundant-vals
                   as sum-val = (+ first-term second-term)
                   never (> sum-val ceiling-value)
                   do (ignore-errors 
                       (setf (bit reference-array (1- sum-val)) 1)))
             (pop abundant-vals))
     ;;reference-array
     (loop for value from 1
           for valid-value across reference-array
           when (zerop valid-value)
             summing value)))
   

;;;; euler 24 needs to be compleated

(defun euler-problem-24 (highest-int nth-term)
  (elt (all-lex-perms (apply #'concatenate 'string
                             (loop for i from 0 to highest-int
                                   collecting (write-to-string i))))
       nth-term)) ;; major issues with the heap exaustion...

(defun euler-24 ()
  (euler-problem-24 9 (1- (expt 10 6))))
;; we could create a generator and have it work until we have the 100th value


(defun euler-problem-25 (number-of-digits)
  (fib-index (lambda (value)
               (>= value (expt 10 (1- number-of-digits))))))

(defun euler-25 ()
  (euler-problem-25 1000))


;;;;;; 26 & 27
(defun euler-problem-26 (denominator-lst)
  (let ((largest-reciprocal-val (reduce #'(lambda (cons1 cons2)
                                            (if (> (cdr cons1) (cdr cons2))
                                                cons1 cons2))
                                        (mapcar (lambda (val)
                                                  (cons val (reciprocal-values val)))
                                                denominator-lst))))
    (values (car largest-reciprocal-val) ;; largest demoninator
            (cdr largest-reciprocal-val)))) ;; numnber of repeating digits
        
(defun euler-26 ()
  (euler-problem-26 (range 2 1000)))

(defun euler-problem-27 (upper-limit-a upper-limit-b)
  (flet ((consecutive-primes (a b)
           (loop for n from 0
                 as quadratic-val = (+ (expt n 2) (* a n) b)
                 when (or (minusp quadratic-val)
                            (not (slow-prime-p quadratic-val)))
                   return n)))
    (loop for a from (- (abs upper-limit-a)) to (abs upper-limit-a)
          with primes-product-cons = '(0 . 0)
          do (loop for b in (collect-primes-under (abs upper-limit-b))
                   as consecutive-primes = (consecutive-primes a b)
                   when (>= consecutive-primes (car primes-product-cons))
                     do (setf primes-product-cons (cons consecutive-primes (* a b))))
          finally (return (cdr primes-product-cons)))))

(defun euler-27 ()
  (euler-problem-27 999 1000))

(defun euler-problem-28 (spiral-dimension)
  (loop with diagonal-generator = (number-spiral-closure)
        with total-sum = 1
        do (multiple-value-bind (diagonal-value edge-length layer-count)
               (funcall diagonal-generator)
             (if (and (> edge-length spiral-dimension)
                      (plusp layer-count))
                 (return total-sum) ;; this is an if
                 (incf total-sum diagonal-value)))))

(defun euler-28 ()
  (euler-problem-28 1001))

(defun generate-multiples-datums (val) ;;; no clear use case beyond this one problem
  (loop for i from val to 999 by val
        collecting (cons i (format nil "~3,'0d" i))))

(defun euler-problem-29 (max-value)
  (loop with distinct-values = nil
        for a from 2 to max-value
        do (loop for b from 2 to max-value
                 do (pushnew (expt a b) distinct-values :test #'=))
        finally (return (values (length distinct-values)
                                distinct-values))))

(defun euler-29 ()
  (euler-problem-29 100))

(defun power-digit-sum (number exponent)
  (reduce #'+ (write-to-string number)
          :key (lambda (digit-char)
                 (expt (digit-char-p digit-char) exponent))))

(defun euler-problem-30 (power upper-limit)
  (loop for i from 2 to upper-limit
        when (= (power-digit-sum i power) i)
          collect i into nth-power-digit-numbers
        finally (return (values (reduce #'+ nth-power-digit-numbers)
                                nth-power-digit-numbers))))

(defun euler-30 ()
  (euler-problem-30 5 (expt 10 6)))

;;; 31-47 tbd

;;; coin sums
(defun coin-sums (total coin-lst)
  (cond
    ((or (minusp total) (null coin-lst)) 0)
    ((zerop total) 1)
    (t (+ (coin-sums (- total (car coin-lst)) coin-lst)
          (coin-sums total (cdr coin-lst))))))

(defun euler-problem-31 (total coin-lst)
  (declare (inline coin-sums))
  (coin-sums total coin-lst))

(defun euler-31 ()
  (euler-problem-31 200 (list 1 2 3 10 20 50 100 200)))


;;; 32

(defun make-all-32-records (pandigital-str)
  (all-lex-perms pandigital-str))

(defun break-in-three (str)
  (loop 
    with tuples-lst = nil 
    for first-bound from 1 to (- (length str) 2)
    as first-term = (subseq str 0 first-bound)
    do (loop for second-bound from (1+ first-bound) to (- (length str) 1)
             as second-term = (subseq str first-bound second-bound)
             as third-term = (subseq str second-bound)
             do (push (list first-term second-term third-term) tuples-lst))
    finally (return tuples-lst)))

(defun euler-32-valid-idenitty (first-term second-term third-term)
  (= (* (read-from-string first-term)
        (read-from-string second-term))
     (read-from-string third-term)))

(defun euler-problem-32 (pandigital-seed-str)
  (let* ((permutations (all-lex-perms pandigital-seed-str))
         (valid-tuples (mapcan (lambda (perm)
                                 (remove-if-not (lambda (tuple)
                                                  (apply #'euler-32-valid-idenitty tuple))
                                                (break-in-three perm)))
                               permutations)))
    (reduce #'+ (remove-duplicates
                 (mapcar #'third valid-tuples)
                 :test #'string=)
            :key #'read-from-string)))

;; euler 33
;;; going to take this chance to play with lisp fraction types
;;; ultimatly want a #'curious-fraction-p fn out of this

(defun invalid-simplify (numerator denominator)
  (assert (and (> numerator 9)   (< numerator 100)
               (> denominator 9) (< denominator 100))
          (numerator denominator))
  (let ((str-numer (write-to-string numerator))
        (str-denom (write-to-string denominator)))
    (ignore-errors ;; excessive and dangerous way to handle divide by 0 errors
     (cond ((char= (aref str-numer 0) (aref str-denom 1))
            (/ (digit-char-p (aref str-numer 1)) (digit-char-p (aref str-denom 0))))
           ((char= (aref str-numer 1) (aref str-denom 0))
            (/ (digit-char-p (aref str-numer 0)) (digit-char-p (aref str-denom 1))))
           (t nil)))))

(defun curious-fraction-p (num denom) ;; should be moved into a cl file for these types
  (let ((simplified-fraction (invalid-simplify num denom)))
    (and simplified-fraction
         (= (/ num denom) simplified-fraction))))

(defun euler-problem-33 () ;; solved for all two digits instead of something more applicable
  (let (curious-fractions)
    (mapcan #'(lambda (denom)
                (mapcar #'(lambda (numer)
                            (when (curious-fraction-p numer denom)
                              (push (list numer denom) curious-fractions)))
                        (range 10 denom)))
            (range 10 99))
    (print (denominator
     (print (reduce #'*
             (print (remove-if (lambda (fraction-pair) ;; remove trivial examples
                         (= (first fraction-pair) (second fraction-pair)))
                      (print curious-fractions)))
            :key (lambda (lst) (apply #'/ lst))
            :initial-value 1))))))

(defun euler-33 ()
  (euler-problem-33))

(defun curious-number-p (number)
  "curious number is one whose value is equal to the sum of the factorial of its digits"
  (assert (> number 9))
  (= number (reduce #'+ (write-to-string number)
                    :key (lambda (char)
                           (factorial (digit-char-p char))))))


;;; setting an upper bound: 9! = 352880
;;; our upper bound is the number of digits where even at all 9 the factorial digit sum is below
;;; the total
;;; 

;;;i        10^i       9!*i
;;;1           10       362880
;;;2          100       725760
;;;3         1000      1088640
;;;4        10000      1451520
;;;5       100000      1814400
;;;6      1000000      2177280
;;;7     10000000      2540160
;;;8    100000000      2903040
;;;9   1000000000      3265920
;;;10  10000000000      3628800

;; based on that table (expt 10 6) will be our upper limit

(defun euler-problem-34 (upper-bound)
  (reduce #'+
          (remove-if-not #'curious-number-p
                             (range 10 upper-bound))))

(defun euler-34 ()
  (euler-problem-34 (expt 10 6)))

(defun all-lex-rotations (str)
  (loop for i from 0 to (1- (length str))
        collecting (concatenate 'string (subseq str i) (subseq str 0 i))))

(defun circular-prime-p (num)
  (every #'(lambda (prime-str)
             (slow-prime-p (read-from-string prime-str)))
         (all-lex-rotations (write-to-string num))))

(defun euler-problem-35 (upper-bound)
  (loop
    with prime-engine = (create-prime-generator)
    as current-prime = (funcall prime-engine)
    until (>= current-prime upper-bound)  
    when (circular-prime-p current-prime)
      collect current-prime into cirular-prime-lst
    finally (return (values (length cirular-prime-lst)
                            cirular-prime-lst))))

(defun euler-35 ()
  (euler-problem-35 (expt 10 6)))

(defun double-base-palindrome (val)
    (and (palindrome-str-p (write-to-string val))
         (palindrome-str-p (write-to-string val :base 2))))

(defun euler-problem-36 (upper-bound)
  (reduce #'+ (remove-if-not #'double-base-palindrome (range 1 upper-bound))))

(defun euler-36 ()
  (euler-problem-36 (expt 10 6)))


(defun truncatable-prime-p (val)
  (flet ((prime-p (val)
           (if (= val 1)
               nil
               (slow-prime-p val))))
      (loop
    with str-val = (write-to-string val)
    for index below (length str-val)
    collecting (subseq str-val index) into prime-strs
    collecting (subseq str-val 0 (- (length str-val) index)) into prime-strs
    finally (return (every #'(lambda (trunc-str)
                               (prime-p (read-from-string trunc-str)))
                           prime-strs)))))

;; lets first attempt to see what happens if we just run until we find all 11 without any
;; work to minimize the search space besides just looking at primes
(defun euler-problem-37 ()
  (loop with prime-engine = (create-prime-generator)
        as prime = (funcall prime-engine)
        when (and (> prime 7) (truncatable-prime-p prime))
          collect prime into valid-prime-lst
        until (> (length valid-prime-lst) 10)
        finally (return (reduce #'+ valid-prime-lst))))

(defun euler-37 ()
  (euler-problem-37))


(defun concatenate-operation (val set fn &key (format-str "~{~d~}"))
  (format nil format-str (mapcar #'(lambda (set-val)
                                   (funcall fn val set-val))
                                 set)))
 
(defun concat-product (val set)
  (declare (inline concatenate-operation))
  (concatenate-operation val set #'*))

;;;38 start off with all the 9 digit pandigital numbers
;; then sort them so that the first match will be our largest match
;; it would be faster to return them on the go but w/e
;;

(defun valid-set (base-val original-str)
  (loop with generated-str = (write-to-string base-val)
        for multiplier in (range 2 9)
        do (setf generated-str (format nil "~a~d" generated-str (* base-val multiplier)))
        never (or (> (length generated-str) (length original-str))
                  (string/= generated-str original-str :end2 (length generated-str)))
        when (string= generated-str original-str)
          return t))


(defun pandigital-multiple (pandigital-str)
  (loop for index from 4 downto 1
        as root-val = (subseq pandigital-str 0 index)
        when (valid-set (read-from-string root-val)
                        pandigital-str)
          return root-val))

(defun euler-38 ()
  (loop for pandigital-str in (all-lex-perms "987654321")
        when (pandigital-multiple pandigital-str)
          return pandigital-str))


(defun chapernows-const-digit (n)
  (loop for i from 1
        as str-i = (write-to-string i)
        when (<= n (length str-i))
          return (digit-char-p (aref str-i (1- n)))
        do (setf n (- n (length str-i)))))



(defun euler-problem-39 (ceiling-val)
  (flet ((count-int-r-tris (p)
           (loop for a from 1 to p
                 with valid-triplet-count = 0
                 do (loop for b from 1 to a
                          as c = (- p a b)
                          when (pythagorean-triplet-p a b c)
                            do (incf valid-triplet-count))
                 finally (return valid-triplet-count))))
  (loop for p from 1 to ceiling-val
        with max-pair = (cons 0 0)
        as cur-count = (count-int-r-tris p)
        when (>= cur-count (cdr max-pair))
          do (setf max-pair (cons p cur-count))
        finally (return (car max-pair)))))


;; (let ((chaper-cur-const ".1") ;;; fun idea but turned out to be abysmally slow on my machine
;;       (cur-counter 1))
;;   (defun get-nth-chapernows-digit (n) ;; closure + error handeling based recursion
;;     (handler-case (digit-char-p (aref chaper-cur-const n))
;;       (sb-int:invalid-array-index-error () (progn (setf chaper-cur-const
;;                                                         (format nil "~a~d"
;;                                                                 chaper-cur-const
;;                                                                 (incf cur-counter)))
;;                                                   (get-nth-chapernows-digit n))))))

(defun euler-project-40 (access-fn pos-lst)
  (reduce #'* pos-lst :key access-fn))

(defun euler-40 ()
  (euler-project-40 #'chapernows-const-digit (loop for i below 7 collecting (expt 10 i))))


(defun euler-41 ()
  ;; i guess hteres an assumption here that a 1 to 10 or higher pandigital number makes no sense
  (loop for i from 9 downto 1
        as pandigital-seed = (format nil "~{~d~}" (reverse (range 1 i)))
        as lexographical-permuatations = (mapcar #'read-from-string (all-lex-perms pandigital-seed))
        as pandigital-primes = (remove-if-not #'slow-prime-p lexographical-permuatations)
        when pandigital-primes
          return (car pandigital-primes)))

(defun euler-problem-42 (list-of-words)
  ;; careful score string requires all words only have aplha characters
  (count-if (lambda (str)
              (triangle-number-p (score-string str)))
            list-of-words))

(defun euler-42 ()
  (with-open-file (file-stream "resources/p042_words.txt" :direction :input)
   (euler-problem-42 (mapcar #'read-from-string (cl-ppcre:split "," (read-line file-stream))))))

(defun euler-43 ()
  (labels ((prime-sub-divisible-p (str)
             (and (zerop (mod (read-from-string (subseq str 1 4)) 2))
                  (zerop (mod (read-from-string (subseq str 2 5)) 3))
                  (zerop (mod (read-from-string (subseq str 3 6)) 5))
                  (zerop (mod (read-from-string (subseq str 4 7)) 7))
                  (zerop (mod (read-from-string (subseq str 5 8)) 11))
                  (zerop (mod (read-from-string (subseq str 6 9)) 13))
                  (zerop (mod (read-from-string (subseq str 7 10)) 17)))))
           
                               
    (reduce #'+ (all-lex-perms "0123456789") :key #'(lambda (str)
                                                    (if (prime-sub-divisible-p str)
                                                        (read-from-string str)
                                                        0)))))

(defun euler-44 () ;;; NEED TO INCREASE SPEED FOR THIS TO BE ACCEPABLE
  (let ((pentagon-numbers)
        (generator (create-pentagon-generator)))
    (loop as next-in-seq = (funcall generator)
          as d-lst = (mapcar (lambda (cached-val)
                               (list (- next-in-seq cached-val)
                                     (+ next-in-seq cached-val)))
                             pentagon-numbers)
          as pent-d-lst = (remove-if-not #'(lambda (lst)
                                             (every #'pentagon-number-p lst))
                                         d-lst)
          when pent-d-lst
            return (sort pent-d-lst #'< :key #'first)
          do (push next-in-seq pentagon-numbers))))

(defun sequence-equality-finder (nth-match &rest number-generators)
  (let ((num-generator-pairs (mapcar (lambda (generator)
                                       (cons (funcall generator) generator))
                                     number-generators)))
    (loop
      when (apply #'= (mapcar #'first num-generator-pairs))
        count 1 into match-counter
      when (= match-counter nth-match)
        return (caar num-generator-pairs) ;; all three are the same, we pick the first one
      do
         
         (setf num-generator-pairs (sort num-generator-pairs #'<= :key #'first))
         (setf (caar num-generator-pairs) (funcall (cdar num-generator-pairs))))))

(defun euler-45 ()
  (sequence-equality-finder 2
                            (create-pentagon-generator)
                            (create-hexogon-generator)
                            (create-triangle-generator)))

(defun composit-number-p (val)
  (declare (inline slow-prime-p))
  (cond ((= val 1) nil)
        ((slow-prime-p val) nil)
        (t t)))

(defun odd-composit-p (val)
  (declare (inline composit-number-p))
  (and
   (oddp val)
   (composit-number-p val)))

(defun golbacks-other-conjecture-p (val)
  (loop for prime in (collect-primes-under val)
        as found-pair = (loop for base from 1 to val
                              as term = (* 2 (expt base 2))
                              when (> (+ term prime) val)
                                return nil
                              when (= (+ term prime) val)
                                return t)
        when found-pair
          return t))

(defun euler-46 ()
  (loop for i from 2
        ;;;do (print i)
        when  (and
               (oddp i)
               (not (slow-prime-p i))
               (not (golbacks-other-conjecture-p i )))
          return i))


;;; 47 - distinct prime factors

(defun euler-problem-47 (distinct-factors consecutive-ints)
  (labels
      ((has-factors (val)
         (= (length (distinct-prime-factors val)) distinct-factors))
       (value-test (val) 
         (every #'has-factors (range val (+ val (1- consecutive-ints))))))
    (loop for i from 2
          when (value-test i)
            return i)))




(defun euler-47 ()
  (euler-problem-47 4 4))

(defun euler-48 ()
  (loop
    with total-sum = 0
    with summation-fn = (limited-digit-add 10)
    for i from 1 to 1000
    do (setf total-sum (funcall summation-fn total-sum (expt i i)))
   finally (return total-sum)))


(defun euler-49 ()
  (loop
    with all-primes = (mapcar #'write-to-string
                              (remove-if (lambda (prime)
                                           (< prime 1000))
                                         (collect-primes-under 10000)))
    for prime-str in all-primes
    as prime-permutations = (all-lex-perms prime-str)
    when (every (lambda (val)
                  (and
                   (not (string= (write-to-string val) "1487"))
                   (member (write-to-string val) all-primes :test #'string=)
                   (member (write-to-string val) prime-permutations :test #'string=)))
                (let ((val (parse-integer prime-str)))
                  (list val
                        (+ val 3330)
                        (+ val 3330 3330))))

      return (concatenate 'string
                           prime-str
                           (write-to-string (+ (parse-integer prime-str) 3330))
                           (write-to-string (+ (parse-integer prime-str) 3330 3330)))))

;; (loop
;;   with ceiling-val = (expt 10 2)
;;   with prime-pool = (collect-primes-under ceiling-val)
;;   with result-lst = '()
;;   for prime-sub-lst on prime-pool
;;   do (print (length prime-sub-lst))
;;      (loop for subsection on (reverse prime-sub-lst)
;;            do (loop for val in subsection
;;                     summing val into total-sum
;;                     when (> val ceiling-val)
;;                       return nil
;;                     finally (when (member total-sum prime-pool :test #'=)
;;                               (push (cons total-sum (length subsection)) result-lst))))
;;   finally (return result-lst))

;; (defvar prime-cons (loop
;;                      for i from 1
;;                      for prime in (collect-primes-under (expt 10 2))
;;                          summing prime into aggregate
;;                          collecting (list i prime aggregate)))

;; (loop
;;   with results = nil
;;   for (upper-n upper-prime  upper-aggregate) in prime-cons   
;;   do (loop for (lower-n lower-prime  lower-aggregate) in prime-cons
;;            as subset-sum = (- upper-aggregate lower-aggregate)
;;            when (or (= upper-n lower-n) (> subset-sum (expt 10 2)))
;;              return nil
;;            when (slow-prime-p subset-sum)
;;              do (push (cons subset-sum (- upper-n lower-n)) results))
;;   finally (return results))


(defun euler-problem-50 (max-value)
    (flet ((consecutive-totals (lst)
         "takes a list of numbers and returns a list where the nth element is equal to the
          sum of the 0-n elements of the original list when an aggregegate-max is given the
          new list cuts off once the total is greater then that val"
               (loop for ele in lst
                     for sub-list-length from 1
                     summing ele into total
                     when (>= total max-value)
                       return sum-lst
                     collecting (cons total sub-list-length) into sum-lst
                     finally (return sum-lst))))
      (let* ((all-primes (collect-primes-under max-value))
             (all-consecutive-prime-totals ; list of consed pairs
               (apply #'append (maplist #'consecutive-totals all-primes)))
             (filtered-prime-tuples (remove 1 all-consecutive-prime-totals :key #'cdr))
             (largest-tuple (find-if #'slow-prime-p
                                     (sort filtered-prime-tuples #'>= :key #'cdr)
                                     :key #'car))
             )
        (values (car largest-tuple) (cdr largest-tuple)))))
