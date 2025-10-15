;; Sample code and try-outs for the book The Little Schemer 4th ed.

(defun atom? (x)
  (not (listp x)))

(setf mithai (list 'ladoo 'barfi 'jalebi))
(setf sweets (list 'ladoo 'barfi 'jalebi))

(defun fact (n)
  (cond ((zerop n) 1)
	(t (* n (fact (- n 1))))))

(defun lat? (l)
  (cond ((null l) t)
	((atom? (car l)) (lat? (cdr l)))
	(t nil)))
