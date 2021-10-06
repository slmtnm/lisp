(defun take (lst n)
  "Takes first n elements from list"
  (if (or (zerop n) (null lst))
      nil
      (cons (car lst) (take (cdr lst) (- n 1)))))

(defun split-by (lst n)
  "Splits list by portions of n elements

  Example:
  (split-by '(1 2 3 4 5) 2)
  >> ((1 2) (3 4) (5))"
  (cond
      ((<= n 0) lst)
      ((<= (length lst) n) (cons lst nil))
      (t (cons (subseq lst 0 n) (split-by (nthcdr n lst) n)))))

(defun flatten (lst)
  "Flattens list and removes nil elements

  Example:
  (flatten '(() 1 (2 (3 ((4)) 5) (6)) (()) 7))
  >> (1 2 3 4 5 6 7)
  "
  (cond
    ((not (listp lst)) (list lst))
    ((null lst) nil)
    (t (apply #'append (mapcar #'flatten lst)))))

(defun bst-preorder (tree)
  "Prints tree in preorder"
  (format t "~a " (car tree))
  (when (second tree) (bst-preorder (second tree)))
  (when (third tree) (bst-preorder (third tree))))
