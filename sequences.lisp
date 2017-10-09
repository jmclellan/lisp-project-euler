(in-package :euler)
;;;;; counld be renames to something other than sequenceces (maby numberic sequences)
(defun fib-assist (n-2 n-1)
  (+ n-2 n-1))

(defun create-fib-generator ()
  (declare (inline fib-assist))
  (let ((n-2 0)
        (n-1 1))
    (lambda ()
      (let ((n (fib-assist n-2 n-1)))
        (setf
         n-2 n-1
         n-1 n)
        n))))


(defun create-prime-generator ()
  (let ((prime-count 0)
        (last-prime 1))
    (lambda ()
      (loop for i from (1+ last-prime)
            until (slow-prime-p i)
            finally (setf last-prime i))
      (values last-prime (incf prime-count)))))


(defun number-spiral-closure ()
  (let ((current-value 1)
        (edge-length 2)
        (layer-count 0))
    (lambda () ;; beautify later
      (incf current-value edge-length)
      (incf layer-count)
      (when (= 4 layer-count)
        (setf layer-count 0
              edge-length (+ edge-length 2)))
      (values current-value
              edge-length
              layer-count))))
