(defun fact (n)
  (loop for i from 2 to n
        and y = 1 then (* y i)
        finally (return y)))

;(defmacro prime-marco-p (p)
;  (let ((p-sym (gensym)))
;    `(let ((,p-sym ,p))
;       (zerop (mod (1+ (fact (- ,p-sym 1))) ,p-sym)))))

(defun primep (p)
  (zerop (mod (1+ (fact (- p 1))) p)))

(defmacro do-primes ((var startv endv) &body body)
  `(loop for i from ,startv to ,endv
         when (primep i) do (let ((,var i)) ,@body)))
