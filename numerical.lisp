;; numerical analysis

(load "funtool.lisp")
(load "mylist.lisp")


;; basic functions
(defun iter-tol (fun &optional (iv 0) (tol 0.001))
	(iter fun iv #'(lambda (x y) (< (abs (- x y)) tol))))

(defun linspace (start stop &optional (no 10))
	"no >= 2, start < stop"
		(if (zerop no) () (cons start (linspace (+ start (/ (- stop start) no)) stop (1- no)))))


;; interpolation
(defun dq (fun nodes)
	(if (equal (length nodes) 1) (funcall fun (car nodes))
	(/ (- (dq fun (cdr nodes)) (dq fun (butlast nodes))) (- (car (last nodes)) (car nodes)))))

(defun ninterp (x nodes fun)
	(let ((x0 (car nodes)))
	(+ (funcall fun x0) (* (ninterp x (cdr nodes) #'(lambda (x) (dq fun (x0 x)))) (- x x0)))))


;; integral
(defun sum (lst) (apply #'+ lst))

(defun trapzoid-sum (lst)
	(let ((a (car lst)) (b (car (last lst))) (rest (cdr (butlast lst))))
	(+ (sum rest) (/ (+ a b) 2))))

(defun trapzoid-int (f a b &optional (n 1))
	(let ((h (/ (- b a) n)) (ydata (mapcar f (linspace a b (1+ n)))))
		(* (trapzoid-sum ydata) h)))

(defun simpson-sum (lst)
	(let ((a (car lst)) (b (car (last lst))) (rest (cdr (butlast lst))))
		(/ (+ a b (* 4 (sum (slice rest :step 2))) (* 2 (sum (slice rest :start 1 :step 2)))) 6)))

(defun simpson-int (f a b &optional (n 1))
	(let ((h (/ (- b a) n)) (ydata (mapcar f (linspace a b (1+ (* n 2))))))
		(* (simpson-sum ydata) h)))


(defun int-func (&key (start 0) (stop 1) (n 100) (int-method #'simpson-int))
	#'(lambda (f) (funcall int-method f start stop n)))

(defun int-op (&key (start 0) (n 100) (int-method #'simpson-int))
	#'(lambda (f) #'(lambda (x) (funcall int-method f start x n))))

;(def err (x) (int-op #'guass))
;; equations
(defun nif (f df)
	"Newton iteration function: x-f(x)/f'(x)"
	#'(lambda (x) (- x (/ (funcall f x) (funcall df x)))))

(defun nfzero (f df &optional (iv 0) (tol 0.001))
	(iter-tol (nif f df) iv tol))

(defun sif (f)
	"iteration function: phi(x,y)=y-f(y)/(f(y)-f(x))*(y-x)"
	#'(lambda (x y) (- y (*(/ (funcall f y) (- (funcall f y) (funcall f x))) (- y x)))))

(defun secant (f &optional (x0 0) (x1 1) (tol 0.001))
	(iter2 (sif f) x0 x1 #'(lambda (x y z) (< (abs (- z y)) tol))))


(defun bisection (f a b n)
	(let* ((fa (funcall f a)) (fb (funcall f b)) (x (/ (+ a b) 2)) (fx (funcall f x)))
		(if (equal n 0) x
	    (if (not (same-sign fa fx)) (bisection f a x (1- n)) (bisection f x b (1- n))))))


;(defun lbase (x xdata &optional (k 0)) )
;(defun linterp (x xdata ydata))
;; example


(disp (funcall (funcall (int-op :n 100) #'guass) 7))



