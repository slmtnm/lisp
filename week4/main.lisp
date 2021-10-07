(defun odd-list-p (lst)
  "Checks whether integer list consists only of odds"
  (every #'(lambda (x) (and (integerp x) (oddp x))) lst))

  (defun sum (lst)
    "Sums list"
    (apply #'+ lst))

(defun replace-odd-lists-by-their-sum (lst)
  "Replaces all sublists, that consists only of odd integers, by their sum"
  (if (consp lst)
      (let ((res (mapcar #'replace-odd-lists-by-their-sum lst)))
        (if (odd-list-p res) (sum res) res))
      lst))

(print (replace-odd-lists-by-their-sum (read)))
