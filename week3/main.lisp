(defun construct-frequency-dictionary (arr)
  (let
    ((dimensions (array-dimensions arr))
     (dictionary '()))
    (dotimes (i (first dimensions) dictionary)
      (dotimes (j (second dimensions))
        (dotimes (k (third dimensions))
          (let*
            ((str (aref arr i j k))
             (e (assoc str dictionary :test #'string=)))
            (if e
                (setf e (incf (cdr e)))
                (push (cons str 1) dictionary))))))))

(defun find-max (assoc-list)
  (let ((m (cdr (first assoc-list))))
    (dolist (pair assoc-list m)
        (when (< m (cdr pair)) (setf m (cdr pair))))))

(print (find-max (construct-frequency-dictionary (read))))
