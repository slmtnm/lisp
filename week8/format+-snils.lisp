(defun string-replace (string pos num by)
  (concatenate 'string
               (subseq string 0 pos)
               by
               (subseq string (+ pos num))))

(defun parse-snils (arg)
  "Returns list of four numbers: '(ABC DEF GHI CC)
   Returns nil if snils is invalid
  "
  (let ((string-arg (if (numberp arg) (write-to-string arg) arg)))
    (when (< (length string-arg) 11)
      (setf string-arg
            (concatenate 'string (make-array (- 11 (length string-arg)) :initial-element #\0)
                         string-arg)))
    (and
     (every #'digit-char-p string-arg)
     (= (length string-arg) 11)
     (list (subseq string-arg 0 3)
           (subseq string-arg 3 6)
           (subseq string-arg 6 9)
           (subseq string-arg 9 11)))))

(defun check-control-sum (snils)
  (let* ((s (concatenate 'string
                        (elt snils 0)
                        (elt snils 1)
                        (elt snils 2)))
        (m (mod (loop for c across s
                      for i = 9 then (1- i)
                      sum (* (digit-char-p c) i))
                101)))
    (eql (parse-integer (elt snils 3)) (mod m 100))))

(defun format-arg-to-snils-string (arg)
  (let ((snils (parse-snils arg)))
    (if (or (null snils) (not (check-control-sum snils)))
        "[bad SNILS]"
        (apply #'format nil "~a-~a-~a-~a" snils))))

(defun format+ (destination control-string &rest format-arguments &aux (arg-num 0))
  (loop for i = (position #\~ control-string)
          then (position #\~ control-string :start (1+ i))
        while i
        when (not (find (elt control-string (1+ i)) "[]{}%"))
          do (incf arg-num)
        when (eql (elt control-string (1+ i)) #\z)
          do (let* ((arg (elt format-arguments (1- arg-num)))
                    (snils-string (format-arg-to-snils-string arg)))
               (setf control-string (string-replace control-string i 2 snils-string))
               (setf format-arguments (remove-if (constantly t) format-arguments :start (1- arg-num) :end arg-num))
               (decf arg-num)))
  (apply #'format destination control-string format-arguments))
