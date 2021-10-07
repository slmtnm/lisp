(defun integer-list-p (lst)
  "Checks whether list consists only of integers"
  (every #'integerp lst))

(defun odd-list-p (lst)
  "Checks whether integer list consists only of odds"
  (every #'oddp lst))

(defun sum (lst &aux (sum 0))
  "Sums list"
  (dolist (l lst sum) (setf sum (+ sum l))))

(defun replace-odd-lists-by-their-sum (lst)
  "Replaces all sublists, that consists only of odd integers, by their sum"
  (cond
    ((not (consp lst)) lst) ; atom or nil
    ((integer-list-p lst) (if (odd-list-p lst) (sum lst) lst)) ; integer-only list
    (t (let ((res (mapcar #'replace-odd-lists-by-their-sum lst))) ; mixed list (some integers, some sublists)
         (if (and (integer-list-p res) (odd-list-p res))
             (sum res)
             res)))))

(print (replace-odd-lists-by-their-sum (read)))
