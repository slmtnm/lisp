;(defmacro symmetric-macrosp (form)
;  (let ((numsym (gensym)) (numstrsym (gensym)))
;    `(let ((,numsym ,form))
;       (and (integerp ,numsym)
;            (> ,numsym 10)
;            (let ((,numstrsym (write-to-string ,form)))
;              (string= ,numstrsym (reverse ,numstrsym)))))))
(defun symmetricp (num)
  (and (integerp num)
       (> num 10)
       (let ((str (write-to-string num)))
         (string= str (reverse str)))))

(defmacro do-sym-nums ((var startv endv) &body body)
  `(loop for i from ,startv to ,endv
         when (symmetricp i) do (let ((,var i)) ,@body)))
