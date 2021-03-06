(in-package :euler)

(defun palindrome-p (val)
  "checks if a number is a palindrome by converting it to a string then doing the comparison"
  (let ((str-val (write-to-string val)))
    (string= str-val (reverse str-val))))

;;; use an ecase to merge these into a single utility
(defun palindrome-str-p (str)
  (string= str (reverse str)))

(defun range (start end &optional (increment 1))
  (loop for i from start to end by increment collecting i))


(defun divisible-by (divisor)
  "returns a lambda which can be used to check if a value is divisible by the
   given divisor"
  (lambda (val) (integerp (/ val divisor))))



(defun nth-triangle-number (n)
  (/ (* n (+ n 1)) 2))

(defun create-triangle-generator ()
  (let ((n 0))
    (lambda ()
      (nth-triangle-number (incf n)))))

(let ((n 1)
      (triangle-cache (list (nth-triangle-number 1))))
      (defun triangle-number-p (val)
        (if (> val (car triangle-cache)) ;; if it is bigger we need to add valued till we can cover it
            (loop as next-triangle-number = (nth-triangle-number (incf n)) ;; destructive modification
                  collecting next-triangle-number into cache-additions
                  when (>= next-triangle-number val)
                    ;; we could improve speed of this fn by getting rid of the call to sort
                    do (setf triangle-cache (sort (append cache-additions triangle-cache) #'>))
                    and return (triangle-number-p val))
            (member val triangle-cache :test #'=))))

(defun collect-divisors (n)
  (loop for possible-divisor from 1 to (sqrt n)
        as result = (/ n possible-divisor)
        when (integerp result)
          collect possible-divisor
          and collect result))


(defun count-divisors (n)
  (loop for possible-divisor from 1 to (sqrt n)
        as result = (/ n possible-divisor)
        when (integerp result)
          summing (if (= possible-divisor (sqrt n)) 1 2)))



(defun next-collatz-val (n)
  (if (evenp n)
      (/ n 2)
      (1+ (* 3 n))))

(defun limited-digit-add (number-of-digits)
  "returns a function what will add and then mod the result so that only the number of
   digits you are interested in will be returned "
  (lambda (&rest summation-args)
    (mod (reduce #'+ summation-args) (expt 10 number-of-digits))))

;;(let ((fn-results (make-array 10000000 :initial-element nil))) ;;million is magic #
;;  (defun collatz-seq (initial-value)
;;    (labels (
;;             (collatz-iter (cur-val &optional current-list)
;;               (if (ignore-errors (aref fn-results cur-val))
;;                   (+ (aref fn-results cur-val)
;;                      (length current-list))
;;                   (let ((next-val (next-collatz-val cur-val)))
;;                     (if (= next-val 1)
;;                         (1+ (length current-list))
;;                         (collatz-iter next-val (cons next-val current-list))
;;                         )))))
;;             (collatz-iter initial-value (list initial-value)))))


;;; could make a memoize macro but not right now
(let ((fn-results (make-array 1000000 :initial-element nil)))
  (defun collatz-length (val)
    (if (ignore-errors (aref fn-results val))
        (aref fn-results val)
        (let ((chain-length (if (= val 1)
                                1
                                (1+ (collatz-length (next-collatz-val val))))))
          (ignore-errors (setf (aref fn-results val) chain-length))
          chain-length))))

(defun factorial (val)
  (loop for i from val downto 2
        with total = 1
        do (setf total (* total i))
        finally (return total)))


(defun fib-index (test-fn)
  (labels ((fib-index-iter (cur-index closure)
             (if (funcall test-fn (funcall closure))
                 cur-index
                 (fib-index-iter (1+ cur-index) closure))))
    (fib-index-iter 2 (create-fib-generator))))


(defun digit-sum (number)
  (loop for digit-char across (write-to-string number)
        summing (digit-char-p digit-char)))


(defun pandigital-test-maker (start end)
  (lambda (value)
    (every (lambda (str-digit)
             (= (count str-digit (format nil "~d" value)) 1))
           (mapcar #'(lambda (int)
                       (code-char (+ int 48)))
                   (range start end)))))


(defun proper-divisors (n)
  (loop for i from 2 to (sqrt n)
        as multiple = (/ n i)
        when (integerp multiple)
          collect i into proper-divisors
          and collect multiple into proper-divisors
        finally (return (cons 1 proper-divisors))))

(defun amicable-number-p (n)
  (let ((digit-sum (reduce #'+ (proper-divisors n))))
    (when (and (/= n digit-sum)
               (= n (reduce #'+ (proper-divisors digit-sum))))
        (values t
                (cons n digit-sum)))))

(defun perfect-number-p (n)
  (= n (reduce #'+ (remove-duplicates (proper-divisors n) :test #'=))))

(defun abundant-number-p (n)
  (< n (reduce #'+ (remove-duplicates (proper-divisors n) :test #'=))))

(defun alpha-pos (char)
  (if (alpha-char-p char)
      (- (char-code (char-upcase char)) 64)
      (error "this function should only be called on letter characters not ~c" char)))

(defun score-string (str)
  "score a string based on the sum of the alpha-positions of each of its letters"
  (reduce #'+ str :key #'alpha-pos))


(defun central-binomial-coefficent (n)
  (/ (factorial (* 2 n))  (expt (factorial n) 2)))

(defun all-lex-perms (str &optional (cur ""))
  "returns all hte lexographical permutations of the string that is passed to it,
the order returned is depended in the order of the characters placed into it"
  (if (string= str "") ;empty-string
      (list cur)
      (reduce #'append str
              :initial-value nil
              :key (lambda (char)
                     (all-lex-perms
                      (remove char str :count 1)
                      (concatenate 'string
                                   cur  
                                   (make-string 1 :initial-element char)))))))

(defun reciprocal-values (divisor &optional (remainder 1) (previous-mods (list 1)))
  "returns the number of repeating digits in the factions (/ 1 divisor)"
  (let ((cur-mod (mod (* remainder 10) divisor)))
    (cond ((zerop cur-mod) 0)
          ((member cur-mod previous-mods :test #'=)
           (1+ (position cur-mod previous-mods :test #'=)))
          (t (reciprocal-values divisor cur-mod (cons cur-mod previous-mods))))))

(defun nth-hexogonal-number (n)
  (* n (- (* 2 n) 1)))

(defun create-hexogon-generator ()
  (let ((n 0))
    (lambda ()
      (nth-hexogonal-number (incf n)))))

(defun nth-pentagon-number (n)
  (/ (* n (- (* 3 n) 1)) 2))

(defun create-pentagon-generator ()
  (let ((n 0))
    (lambda ()
      (nth-pentagon-number (incf n)))))


(let ((n 0)
      (pentagon-cache))
  (defun pentagon-number-p (val)
    (if (or (null pentagon-cache) (> val (car pentagon-cache)))
        ;; if it is bigger we need to add valued till we can cover it
        (loop as next-pentagon-number = (nth-pentagon-number (incf n)) ;; destructive modification
              collecting next-pentagon-number into cache-additions
              when (>= next-pentagon-number val)
                ;; we could improve speed of this fn by getting rid of the call to sort
                do (setf pentagon-cache (sort (append cache-additions pentagon-cache) #'>))
                and return (pentagon-number-p val))
        (member val pentagon-cache :test #'=))))


