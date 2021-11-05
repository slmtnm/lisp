(defun string-replace (string pos num by)
  "Replaces substring (from pos to pos+num) of given string
   with another string"
  (concatenate 'string
               (subseq string 0 pos)
               by
               (subseq string (+ pos num))))

(defun to-string (lst)
  "Transforms all numeric elements to string
   string element are untouched
   other types to nil"
  (mapcar #'(lambda (x)
              (cond
                ((numberp x) (write-to-string x))
                ((and (stringp x) (every #'digit-char-p x)) x)
                (t nil))) lst))

(defun digits-to-string (str)
  (if (= (length str) 7)
      (format nil "~a-~a-~a"
              (subseq str 0 3)
              (subseq str 3 5)
              (subseq str 5 7))
      str))

(defun phone-number-to-string (arg)
  "Parameter: list of 3 elements, each element either string or number
   Returns number as a string
   Returns [bad phone] if phone number is invalid"
  (setq arg (to-string arg))
  (cond
    ((some #'null arg) "[bad phone]") ; some of arguments neigher number nor string
    ((> (apply #'+ (mapcar #'length arg)) 15) "[bad phone]") ; overall number length > 15
    (t (format nil "+~a(~a)~a"
               (elt arg 0)
               (elt arg 1)
               (digits-to-string (elt arg 2))))))

(defun format+ (destination control-string &rest format-arguments &aux (arg-num 0))
  (loop for i = (position #\~ control-string)
            then (position #\~ control-string :start (1+ i))
        while i
        when (eql (elt control-string (1+ i)) #\z)
          do (let* ((arg (elt format-arguments arg-num))
                    (phone-string (phone-number-to-string arg)))
               (setf control-string (string-replace control-string i 2 phone-string))
               (setf format-arguments (remove-if (constantly t) format-arguments :start arg-num :end (1+ arg-num))))
        when (not (find (elt control-string (1+ i)) "[]{}%")) do (incf arg-num))
  (apply #'format destination control-string format-arguments))
