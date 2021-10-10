(defun telephone-p (str)
  "Checks whether given string is a valid telephone number (internal or external)"
  (cond ((= (length str) 4) (every #'digit-char-p str)) ; internal phone number
        ((and (> (length str) 1)
              (<= (length str) 16)
              (eql (elt str 0) #\+)) ; external phone number
         (every #'digit-char-p (subseq str 1)))
        (t nil)))

(defun parse-cdr (str)
  "Splits colon-separated CDR-string into list of three elements or nil if CDR invalid"
  (let* ((first-colon (position #\; str))
         (second-colon ()))
    )
  (let ((first-colon (position #\; str)))
    (if (null first-colon)
        nil
        (let ((second-colon (position #\; str :start (1+ first-colon))))
          (if (null second-colon)
              nil
              (list (string-left-trim " " (subseq str 0 first-colon))
                    (string-left-trim " " (subseq str (1+ first-colon) second-colon))
                    (string-left-trim " " (subseq str (1+ second-colon)))))))))

(defun cdr-p (cdr)
  "Checks whether triplet (list of 3 elements) is a valid CDR
  First and second elements are telephone numbers
  Third element is call duration, which must be non-zero"
  (and (telephone-p (first cdr))
       (telephone-p (second cdr))
       (every #'digit-char-p (third cdr))
       (/= 0 (parse-integer (third cdr)))))

(defun starts-with-one-of (str strs)
  "Checks whether string 'str' starts with one of strings in 'strs' list"
  (some
   #'(lambda (s)
       (string= s (subseq str 0 (length s))))
   strs))

(defun hash-to-pairs (h &aux result)
  "Converts hash table to list of pairs"
  (maphash #'(lambda (k v) (push (list k v) result)) h)
  result)

(defun hash++ (key h)
;  (let ((value (gethash key h)))
;    (when (null value) (setf value 0))
;    (incf value)))
  (when (null (gethash key h)) (setf (gethash key h) 0))
  (incf (gethash key h)))

(defun cdrs-call-frequency (cdrs region-prefix)
  "Calculates frequncy of calls to numbers with 'region-prefix'"
  (hash-to-pairs
   (reduce
    #'(lambda (h cdr)
        (when (starts-with-one-of (second cdr) (list region-prefix))
          (hash++ (first cdr) h))
        h)
    cdrs
    :initial-value (make-hash-table :test #'equal))))

(defun most-frequent-caller-to (cdrs region-prefix)
  (caar
   (sort
    (cdrs-call-frequency cdrs region-prefix)
    #'>
    :key #'second)))

(defun sum-incoming-durations (cdrs worker-number)
  (reduce
   #'(lambda (acc c)
       (if (string= worker-number (second c)) (+ acc (parse-integer (third c))) acc))
   cdrs :initial-value 0))

(defun main (cdrs-str)
  (let* ((cdrs (remove-if-not #'cdr-p (mapcar #'parse-cdr cdrs-str)))
         (brazil-caller (most-frequent-caller-to cdrs "+55")))
    (sum-incoming-durations cdrs brazil-caller)))

(format t "~a~%" (main (read)))
