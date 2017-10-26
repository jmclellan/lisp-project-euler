(in-package :euler)

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
                   do (setf max-digit-sum cur-digit-sum)
                 )
        finally (return max-digit-sum)))

(defun euler-toitent-function (val)
  (reduce #'* (slow-prime-factorization val) :key #'1-))

(defun euler-problem-69 (ceiling)
  (loop for i from 2 to ceiling
        with max-i = 0
        with ratio = 0
        as toitent = (euler-toitent-function i)
        as cur-ratio = (/ i toitent)
        do (format t "~&~a ~a ~a" i toitent cur-ratio)
        when (> cur-ratio ratio)
          do (setf max-i i
                   ratio cur-ratio)
        finally (return max-i)))

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

;;; for 72 i think i can use the toitent function once it is fixed up

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
  (reduce #'+ (write-to-string val) :key (lambda (digit-char)
                                           (factorial (digit-char-p digit-char)))))




















