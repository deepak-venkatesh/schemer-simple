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

(defun my-member (a lat)
  (cond ((null lat) nil)
	((equal a (car lat)) t)
	(t (my-member a (cdr lat)))))

(defun member? (a lat)
  (cond ((null lat) nil)
	(t (or (equal (car lat) a) (member? a (cdr lat))))))

(defun rember-wrong (a lat)
  (cond ((null lat) nil)
	((equal a (car lat)) (cdr lat))
	(t (rember-wrong a (cdr lat)))))

(defun rember (a lat)
  (cond ((null lat) nil)
	((equal a (car lat)) (cdr lat))
	(t (cons (car lat)
		 (rember a (cdr lat))))))
