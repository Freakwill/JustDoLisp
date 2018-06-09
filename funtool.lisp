;; tools of functions


(defun show (x) (format t "~a" x))
(defun disp (x) (format t "~f" x))


;; baisc functions
(defun not-null (x) (not (null x)))

(defun id (x) x)

(defun chi (x lst) (if (member x lst) 1 0))

(defun chi-interval (x a b) (if (<= a x b) 1 0))

(defun bool2int (b) (if b 1 0))

(defun one (x) 1)

(defun zero (x) 0)

(defun constant (x &optional (c 0)) c)

(defun inv (x) (/ 1 x))

(defun squre (x) (expt x 2))

(defun if-else (x y &optional z) (if x y z))


(defun if-elif-else (x1 y1 x2 y2 &optional z)
	(cond (x1 y1)
		(x2 y2)
    	(t z)))


;; special functions
(defun heaviside (x &optional (mid 1))
	"Heaviside step function"
	(cond ((plusp x) 1)
	((minusp x) 0)
	(t mid)))

(defun sigmoid (x)
	"S-type activation function"
	(inv (1+ (exp (- x)))))

(defun diff-sigmoid (x)
	"derivative of sigmoid"
	(let ((y (sigmoig x)))
	(* y (1- y))))

(defun hardlim (x) (heaviside x))

(defun ramp (x &optional (c 1))
	"ramp function"
	(cond ((<= x (- c)) -1)
		((< (- c) x c) (/ x c))
		(t 1)))

(defun max0 (x) (max x 0))

(defun guass (x) (exp (- (squre x))))

(defun runge (x) (/ 1 (1+ (squre x))))

;(defun poly (x &optional lst (z (car lst)))
;	(cond ((null lst) 0)
;		((equal (length lst) 1) z)
;	    (t (poly x (cdr lst) (+ (cadr lst) (* x z))))))

(defun poly (x &optional lst)
	"anx^n + an-1x^n-1 + ...  a0"
	(if (null lst) 0
	(+ (car (last lst)) (* x (poly x (butlast lst))))))

(defun poly-asceding (x &optional lst)
	"a0 + a1x + a2x^2 + ..."
	(if (null lst) 0
	(+ (car lst) (* x (poly-asceding x (cdr lst))))))

;;
(defun same-sign (&rest nums)
	(or (every #'plusp nums) (every #'minusp nums) (every #'zerop nums)))


;; advanced functions (functionals)
(defun delta (x) #'(lambda (f) (funcall f x)))

(defun const (a) #'(lambda (x) a))

(defun dil (f &optional (k 1))
	#'(lambda (x) (funcall f (- x k))))

(defun compose (&rest funs)
	(let ((N (length funs)))
	(cond ((zerop N) #'id)
	    ((equal 1 N) (car funs))
	    ((equal 2 N) #'(lambda (x) (funcall (car funs) (funcall (cadr funs) x))))
	    (t (compose (car funs) (apply #'compose (cdr funs)))))))

(defun fadd (&rest funs)
	(let ((N (length funs)))
	(cond ((zerop N) 0)
	    ((equal 1 N) (car funs))
	    ((equal 2 N) #'(lambda (x) (+ (funcall (car funs) x) (funcall (cadr funs) x))))
	    (t (fadd (car funs) (apply #'fadd (cdr funs)))))))

(defun fsmul (fun a)
	#'(lambda (x) (* a (funcall fun x))))

(defun fsadd (fun a)
	#'(lambda (x) (+ a (funcall fun x))))

(defun flist (&rest funs)
	#'(lambda (x) (mapcar (delta x) funs)))

(defun ftensor (f &optional (g f))
	#'(lambda (x y) (* (funcall f x) (funcall g y))))


(defun partial (fun &rest args)
	"lmd xs: fun(xs, args)"
	#'(lambda (&rest xs) (apply fun (append xs args))))

(defun curry (fun &rest args)
	"lmd xs: fun(args, xs)"
	#'(lambda (&rest xs) (apply fun (append args xs))))

; template

(defun tail-rec (lst fun &optional ret)
	(if (null lst) ret
	(funcall fun (car lst) (tail-rec (cdr lst) fun ret))))

(defun iter0 (fun &optional (iv 0) (condition #'(lambda (x) t)))
	(if (funcall condition iv) iv (iter fun (funcall fun iv) condition)))

(defun iter (fun &optional (x0 0) (condition #'(lambda (x y) t)))
	(let ((x1 (funcall fun x0)))
	(if (funcall condition x0 x1) x1 (iter fun x1 condition))))


(defun iter2 (fun &optional (x0 0) (x1 1) (condition #'(lambda (x y z) t)))
	(let ((x2 (funcall fun x0 x1)))
	(if (funcall condition x0 x1 x2) x2 (iter2 fun x1 x2 condition))))


;(show (funcall (ftensor #'1+ #'id) 1 2))
(show (funcall (curry #'if-else nil 2) 1))