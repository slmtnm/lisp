(defun valid-char (c)
  (or (digit-char-p c)
      (char= #\. c)
      (char= #\X c)
      (char= #\_ c)))

(defun stringify-stream (stream)
  "Breaks stream into char list"
  (symbol-name (read stream)))

(defun safe-parse-integer (str)
  (with-input-from-string (in str)
    (let ((value (read in)))
      (typecase value
        (integer value)
        (t nil)))))

(defun safe-parse-number (str)
  (with-input-from-string (in str)
    (let ((value (read in)))
      (typecase value
        (number value)
        (t nil)))))

(defun retrieve-initial (str &aux underscore-pos)
  "Returns three values:
   1. parsed initial values (integer)
   2. rest string
   3. t or nil - whether parsing initial value was successful"
  (setf underscore-pos (position #\_ str))
  (if underscore-pos
      (let ((parsed-integer (safe-parse-number (subseq str (1+ underscore-pos)))))
        (if parsed-integer
            (values (safe-parse-number (subseq str (1+ underscore-pos)))
                    (subseq str 0 underscore-pos)
                    t)
            (values nil nil nil)))
      (values 0 str t)))

(defun split-string (str char)
  (loop for i = 0 then (1+ j)
        as j = (position char str :start i)
        collect (subseq str i j)
        while j))

(defun retrieve-dims (str &aux dims)
  "Returns either list of dimensions or nil"
  (let ((dim-strings (split-string str #\X)))
    (when (position "" dim-strings :test #'string-equal)
      (return-from retrieve-dims nil))
    (loop for dim-string in dim-strings
          do (let ((dim (safe-parse-integer dim-string)))
               (when (null dim) (return-from retrieve-dims nil))
               (when (<= dim 0) (return-from retrieve-dims nil))
               (push dim dims)))
    (nreverse dims)))

(defun retrieve-dims-and-initial (str)
  (multiple-value-bind (initial rest result) (retrieve-initial str)
    (when (null result) (return-from retrieve-dims-and-initial nil))
    (let ((dims (retrieve-dims rest)))
      (when (null dims) (return-from retrieve-dims-and-initial nil))
      (list dims initial))))

(defun macro (stream c1 c2 &aux res)
  (setf res (retrieve-dims-and-initial (stringify-stream stream)))
  (when (null res) (return-from macro nil))
  (destructuring-bind (dims initial) res
    `(make-array (list ,@dims) :initial-element ,initial)))

(set-dispatch-macro-character #\# #\M #'macro)
