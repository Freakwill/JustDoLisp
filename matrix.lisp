(load "D:\\clisp\\mylisp\\mylist.lisp")


;; matrix-like list
(defun dot (v u)
	(sum (mapcar #'* v u)))

(defun mul' (A B)
	(if (null A) ()
	(cons (mapcar #'(lambda (x) (dot (car A) x)) B) (mul' (cdr A) B))))

(defun mul (A B)
	(mul' A (transpose B)))

(defun cat2 (A B)
	(cond ((null A) B)
		((null B) A)
	    (t (mapcar #'append A B))))

(defun transpose (A)
	(if (null A) ()
	(cat2 (mapcar #'list (car A)) (transpose (cdr A)))))


(defun transp (A)
	"same to transpose, but call tail-rec"
	(tail-rec A #'(lambda (x y) (cat2 (mapcar #'list x) y))))


(defun print_vec (v)
	(if (null v) nil
     (show v) (print_vec (cdr v))))

(defun print_mat (A)
	(dolist (v A)
    (print_vec v)))

(defun matrix-like (A)
	(and (listp A) (same-by-key A #'(lambda (x) (length x)))))
    