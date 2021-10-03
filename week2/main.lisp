(defun number-of-bytes (n)
  (ceiling (integer-length n) 8))

(defun invert-if-lower-one (n)
  (if (= 1 (logand n 1)) (logxor 12 n) n))

(defun main (x)
  (dotimes (i (number-of-bytes x) x)
    (let* (
        (b (ldb (byte 8 (* i 8)) x))
        (lower (ldb (byte 4 0) b))
        (upper (ldb (byte 4 4) b))
        (result (+ (invert-if-lower-one lower) (ash (invert-if-lower-one upper) 4))))
      (setf x (dpb result (byte 8 (* i 8)) x)))))

(print (main (read)))
