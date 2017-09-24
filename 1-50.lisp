(in-package :euler)

(defun euler-problem-1 (ceiling-value)
  (loop for i from 1 to ceiling-value
        when (or (zerop (mod i 3)) (zerop (mod i 5)))
          summing i))

(defun euler1 ()
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
  (euler-problem-3 60085147514))
  

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



;;;(defun euler-problem-9 (goal-value)
;;;    (loop for a from 1
;;;      with triplets = nil
;;;          until (> a goal-value)
;;;      do (loop for b from (1+ a)
;;;               until (> (+ a b) goal-value)
;;;               do (loop for c from (1+ b)
;;;                        until (> (+ a b c) goal-value)
;;;                        when (= (+ a b c ) goal-value)
;;;                          do (push (list a b c) triplets)))
;;;          finally (return (apply #'* (car (remove-if-not #'(lambda (trip)
;;;                                     (apply #'pythagorean-triplet-p trip))
;;;                                 triplets))))))
                     

(defun euler-9 ()
  (euler-problem-9 1000 1000))

(defun euler-problem-10 (ceiling-value)
  (reduce #'+ (collect-primes-under ceiling-value)))

(defun euler-10 ()
  (euler-problem-10 (expt 10 6)))


(defun generate-indicies (seed-index)
  (values
   (list seed-index (+ seed-index 20) (+ seed-index 40) (+ seed-index 60))
   (list seed-index (+ seed-index 1)  (+ seed-index 2)  (+ seed-index 3))
   (list seed-index (+ seed-index 21) (+ seed-index 22) (+ seed-index 23))))
;;; several of these indecies will be out of bounds or not adjacent, we deal with
;;; in the next function

(defun search-euler-11-space ()
  (loop for index below (length +euler-11-data+)
        with all-products = '()
        with index->product = (lambda (&rest indicies)
                                (ignore-errors
                                 (reduce #'*
                                         (mapcar (lambda (i)
                                                   (aref +euler-11-data+ i))
                                                 indicies))))
        do (multiple-value-bind (vertical-indicies
                                 horizontal-indicies
                                 diagonal-indicies)
               (generate-indicies index)
             (push (apply index->product vertical-indicies) all-products)
             (when (< (mod index 20) 17)
               (push (apply index->product horizontal-indicies) all-products))
             (when (and (< (mod index 20) 17)
                        (< index 340)) ;; check if diagonal will be in adjacent
               (push (apply index->product diagonal-indicies) all-products))
             )
        finally (return (reduce #'max (remove nil all-products)))))

(defun euler-11 ()
  (search-euler-11-space))


;; bug in above - will collect sqrt twice ie for 9 3 will appear twice





(defun euler-problem-12 (divisor-count)
 (loop for n from 1
      as cur-tri-num = (nth-triangle-number n)
      when (>= (count-divisors cur-tri-num) divisor-count)
        return cur-tri-num))

(defun euler-12 ()
  (euler-problem-12 500))




(defun euler-problem-13 (value-array)
  (mod (reduce #'+ value-array) 10000000000))

(defun euler-13 ()
  (euler-problem-13 +euler-13-data+))


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

;;;; do 19

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


;;(let ((days (make-circular '("MON" "TUE" "WED"
;;                             "THU" "FRI" "SAT"
;;                             "SUN")))
;;      (months (make-circular '("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
;;                               "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))))
;;      (loop for year from 1900
;;        with days-of-note = 0
;;            do (loop as month = (pop months)
;;                     do (loop for day from 1
;;                              as day-of-week = (pop days)
;;                              when (< day (days-in-month month year))
;;                                return nil
;;                              do (format t "day: ~3d Month: ~a Year: ~4d"
;;                                         day month year)
;;                              )
;;        )))


;;;;
;;(defun hi-jill ()
;;  (format t "~&hey, your cute"))

(defun euler-problem-20 (val)
  (digit-sum (factorial val)))

(defun euler-20 ()
  (euler-problem-20 100))


(defun euler-problem-21 (ceiling-value)
  (loop for i from 1 to ceiling-value
        when (amicable-number-p i)
          sum i))

(defun euler-21 ()
  (euler-problem-21 (expt 10 5)))

;;; euler 22
;;;; use score-string function and map it across the sorted list of names with a closure which will then cause everyting to work out
(defun euler-problem-22 (str-lst)
  (reduce #'+ (sort str-lst #'string<=) :key (let ((counter 0))
                                                       (lambda (word)
                                                         (* (score-string word)
                                                            (incf counter))))))

(defun euler-22 ()
  (euler-problem-22 +euler-21-data+))

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
             (reference-array (make-array ceiling-value :element-type 'bit))
             
             )
        (loop while abundant-vals
              as first-term = (pop abundant-vals)
              do (loop for second-term in abundant-vals
                       as sum-val = (+ first-term second-term)
                       never (> sum-val ceiling-value)
                       do (ignore-errors 
                           (setf (bit reference-array (1- sum-val)) 1))
                       ))
    reference-array
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
  (euler-problem-24 9 (expt 10 6)))
;; we could create a generator and have it work until we have the 100th value


(defun euler-problem-25 (number-of-digits)
  (fib-index (lambda (value)
               (>= value (expt 10 number-of-digits)))))

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

(defun euler-problem-27 (upper-limit)
  (flet ((consecutive-primes (a b)
           (loop for n from 0
                 as quadratic-val = (+ (expt n 2) (* a n) b)
                 unless (or (minusp quadratic-val) (slow-prime-p quadratic-val))
                   return n)))
    ;;;(format t "~& a: ~a b: ~a result: ~a" -71 1601 (consecutive-primes -79 1601))
    (loop for a from (- (abs upper-limit)) to (abs upper-limit)
          with primes-product-cons = '(0 . 0)
          do (loop for b from (- (abs upper-limit)) to (abs upper-limit)
                   as consecutive-primes = (consecutive-primes a b)
                   ;;do (format t "~%~4d ~5d ~a" a b consecutive-primes)
                   when (> consecutive-primes (car primes-product-cons))
                     do (setf primes-product-cons (cons consecutive-primes
                                                        (* (abs a) (abs b)))))
          finally (return (cdr primes-product-cons)))))

(defun euler-27 ()
  (euler-problem-27 999))

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

(defun euler-problem-48 (ceiling-value)
  (loop for i from 1 to ceiling-value
        summing (expt i i)))

(defun euler-48 ()
  (mod (euler-problem-48 1000) (expt 10 10)))

;;; 47 - distinct prime factors

(defun euler-problem-47 (distinct-factors consecutive-ints)
  (labels
      ((has-factors (val)
         (= (length (distinct-prime-factors val)) distinct-factors))
       (value-test (val) 
         (every #'has-factors (range val (+ val (1- consecutive-ints))))))
    (loop for i from 1
          when (value-test i)
            return i)))


(defun euler-47 ()
  (euler-problem-47 4 4))


;;(defun euler-problem-48 (ceiling-self-power scaling-fn)
;; (loop for i from 1 to ceiling-self-power
;;       with total-sum = 0
;;        do (setf total-sum (funcall scaling-fn (+ total-sum (expt i i))))
;;        finally (return total-sum)))

;;(defun euler-48 ()
;;  (euler-problem-48 1000 (lambda (val) (mod val (expt 10 10)))))


;;(time (remove-if (lambda (v) (< v 1000)) (collect-primes-under (expt 10 4))))

;;(time (let* ((valid-primes (remove-if (lambda (v) (or (< v 1000)
;;                                                      (not (slow-prime-p v))))
;;                                      (collect-primes-under (expt 10 4))))
;;             (prime-datums (mapcar (lambda (prime)
;;                                     (list prime
;;                                           (sort (write-to-string prime) #'char<)))
;;                                   valid-primes)))
;;        (print prime-datums)
;;        (mapcar #'(lambda (prime-datum)
;;                    (cons (count-if (lambda (datums)
;;                                      (string= (second prime-datum) (second datums));;) 
;;                                    prime-datums) prime-datum)) prime-datums)
;;        t))

