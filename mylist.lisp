;; list processing
(load "D:\\clisp\\mylisp\\funtool.lisp")

;; notation:
;; lst: list
;; a: s-expression (or atom)
;; lists: list of list


;; index of list
(defun getitem (lst &optional index)
	(cond ((null index) ())
		((integerp index) (getitem1 lst index))
		(t (cons (getitem1 lst (car index)) (getitem lst (cdr index))))))

(defun getitem1 (lst index)
    (nth (if (>= index 0) index (+ (length lst) index)) lst))

(defun slice (lst &key (start 0) (stop (length lst)) (step 1))
	"slice of list"
	(cond ((> step 0) (if (>= start stop) ()
	    (cons (nth start lst) (slice lst :start (+ start step) :stop stop :step step))))
		((< step 0) (if (<= start stop) ()
		(cons (nth start lst) (slice lst :start (+ start step) :stop stop :step step))))
		(t ())))

(defun last-one (lst) (car (last lst)))

;(defun setitem (lst &optional (index 0) (value 0))
;	(setf (aref lst index) value))

(defun insert (lst &key (element 0) (loc 0))
	(if (zerop loc) (cons element lst) (cons (car lst) (insert (cdr lst) :loc (1- loc) :element element))))

;; operate list
(defun list-add (lst a) (append lst (list a))) ;; [a b c] + sx  |-> [a b c sx]

(defun cat (lists)
	"see concatenate"
	(if (null lists) () (apply #'append lists)))
;; another verion of cat:
;; (defun cat (lists) (apply #'append lists))


(defun list-replace (lst pairs)
	"replace the element in list according to pairs"
	(if (null pairs) lst
	(list-replace (list-replace1 lst (car pairs)) (cdr pairs)))
)

(defun list-replace1 (lst pair)
	"replace the element in list according to one pair"
	(if (null lst) ()
		(substitute (cadr pair) (car pair) lst)
    )
)

(defun list-sub (lst pairs)
	"replace the element in list according to pairs parallelly"
	(let ((pair (assoc (car lst) pairs :test #'equal)))
	(if (null lst) ()
	(cons (if pair (cadr pair) (car lst)) (list-sub (cdr lst) pairs))))
)

(defun assoc-list (lst pairs)
	"see assoc"
	(let ((pair (assoc (car lst) pairs :test #'equal)))
		(cond ((null lst) ())
		(pair (cons (cadr pair) (assoc-list (cdr lst) pairs)))
		(t (assoc-list (cdr lst) pairs))
		)
	)
)

(defun find-all (a lst &optional (key #'equal))
	"find all a-s in a list"
	(cond ((null lst) ())
	((funcall key a (car lst)) (cons 0 (mapcar #'1+ (find-all a (cdr lst) key))))
	(t (mapcar #'1+ (find-all a (cdr lst))))))

(defun classify (a &optional lst (key #'member))
	"only one element in lst satisfies (key a element)"
	(if (null lst) (list (list a))
	    (if (funcall key a (car lst)) (cons (cons a (car lst)) (cdr lst))
	    (cons (car lst) (classify a (cdr lst))))))

(defun partition (&optional lst (rel #'equal))
	"partition of a list, rel: equivalent relation"
	(if (null lst) ()
	(classify (car lst) (partition (cdr lst) rel) #'(lambda (x y) (funcall rel x (car y))))))

(defun sort-by-key (lst &optional (key #'id))
	(sort lst #'(lambda (x y) (funcall key x) (funcall key y))))

(defun sort-by-len (lst)
	(sort-by-key lst #'length))


(defun min-by-key (lst &optional (key #'id))
	(apply #'min (mapcar key lst)))

(defun cut (lst k)
	(list (slice lst 0 k) (nthcdr k lst)))


(defun car-cdr (lst)
	(list (car lst) (cdr lst)))

(defun shift (lst &optional (k 1))
	(let* ((L (length lst)) (kk (mod k L)))
	(if (or (<= L 1) (equal kk 0)) lst
	    (append (nthcdr kk lst) (slice lst 0 kk)))))

;; generate list
(defun range (start stop &optional (step 1))
	(cond ((> step 0) (if (>= start stop) ()
	    (cons start (range (+ start step) stop step))))
		((< step 0) (if (<= start stop) ()
		(cons start (range (+ start step) stop step))))
		(t ())))

;(defun linspace (start stop &optional (step 1))
;	(cond ((> step 0) (if (> start stop) ()
;	    (cons start (linspace (+ start step) stop step))))
;		((< step 0) (if (< start stop) ()
;		(cons start (linspace (+ start step) stop step))))
;		(t ())))

;; judgement about list
(defun beginwith (lst1 &optional lst2)      ;; lst1 = list2 + [...]
	"(a b ...) beginwith (a b)"
	(cond ((null lst2) t)
		((equal (car lst1) (car lst2)) (beginwith (cdr lst1) (cdr lst2)))
		(t nil)))


(defun sublist (lst1 &optional lst2)
	"judge whether lst1 is a sublist of lst2"
    (let ((L1 (length lst1)) (L2 (length lst2)))
	    (cond ((> L1 L2) ())
	    	((null lst1) (range 0 L2 1))
	        ((beginwith lst2 lst1) (cons 0 (mapcar #'1+ (sublist lst1 (cdr lst2)))))
	        (t (mapcar #'1+ (sublist lst1 (cdr lst2))))))
)

(defun all (&optional lst (key #'not-null))
	"see EVERY"
	(if (null lst) t
	    (and (funcall key (car lst)) (all (cdr lst)))))

(defun any (&optional lst (key #'not-null))
	(if (null lst) nil
	    (or (funcall key (car lst)) (any (cdr lst)))))

(defun chain (&optional lst (cmp #'<))
	(if (<= (length lst) 1) t
	(and (funcall cmp (car lst) (cadr lst)) (chain (cdr lst) cmp))))

(defun same (&optional lst)
	(if (<= (length lst) 1) t
	(if (equal (car lst) (cadr lst)) (same (cdr lst)) nil)))


(defun same-by-key (&optional lst (key #'id))
	(same (mapcar key lst)))


(defun nth-by-key (lst &optional (key #'not-null) (nth 0))
	"same with find-if"
	(let ((head (car lst)) (tail (cdr lst)))
	(if (zerop nth) (find-if key lst)
	    (if (funcall key head) (nth-by-key tail key (1- nth))
	        (nth-by-key tail key nth)))))

(defun isunique (&optional lst (key #'id))
	(let ((head (car lst)) (tail (cdr lst)))
	(if (null lst) t
	    (if (member head tail) nil (isunique tail)))))

;; number-list
(defun list<= (&optional lst1 lst2)
	(let ((a1 (car lst1)) (a2 (car lst2)))
	(cond ((null lst1) t)
		((null lst2) nil)
	    ((< a1 a2) t)
	    ((equal a1 a2) (list<= (cdr lst1) (cdr lst2)))
	    (t nil))))

(defun list< (&optional lst1 lst2)
	(let ((a1 (car lst1)) (a2 (car lst2)))
	(cond ((null lst2) nil)
		((null lst1) t)
		((< a1 a2) t)
	    ((equal a1 a2) (list< (cdr lst1) (cdr lst2)))
	    (t nil))))

(defun list>= (&optional lst1 lst2)
	(let ((a1 (car lst1)) (a2 (car lst2)))
	(cond ((null lst2) t)
		((null lst1) nil)
	    ((> a1 a2) t)
	    ((equal a1 a2) (list>= (cdr lst1) (cdr lst2)))
	    (t nil))))

(defun listcmp (&optional lst1 lst2 (cmp #'<))
	(let ((a1 (car lst1)) (a2 (car lst2)))
	(cond ((null lst2) nil)
		((null lst1) t)
		((cmp a1 a2) t)
	    ((equal a1 a2) (listcmp (cdr lst1) (cdr lst2) cmp))
	    (t nil))))

;; set-like list
(defun set+ (lst a)
	(if (member a lst) lst (list-add lst a)))

(defun set- (lst a)
	(let ((x (car lst)) (rest (cdr lst)))
	(cond ((null lst) ())
	    ((equal a x) (set- rest a))
	    (t (cons x (set- rest a)))
	))
)

(defun setu (lst1 lst2)
	(if (null lst2) lst1
		(setu (set+ lst1 (car lst2)) (cdr lst2))))

(defun setn (list1 list2)
	(let ((a (car list1)) (rest (cdr list1)))
	(cond ((or (null list1) (null list2)) ())
		((member a list2) (cons a (setn rest list2)))
		(t (setn rest list2))))
)

(defun set^ (list1 list2)
	(setu (set-difference list1 list2) (set-difference list2 list1)))

(defun unique (lst)     ;; [1 2 3 1 3]  => [1 2 3]
	(cond ((null lst) ())
	    ((member (car lst) (cdr lst)) (unique (cdr lst))) 
        (t (cons (car lst) (unique (cdr lst)))))
)

(defun card (lst)     ;; [1 2 3 1 3]  => [1 2 3]
	(cond ((null lst) 0)
	    ((member (car lst) (cdr lst)) (card (cdr lst))) 
        (t (1+ (card (cdr lst)))))
)

(defun set<= (list1 list2)
	(cond ((null list1) t)
	    ((member (car list1) list2) (set<= (cdr list1) list2))
	    (t nil)))

(defun set< (list1 list2)
	(cond ((null list2) nil)
		((null list1) t)
	    ((member (car list1) list2) (set< (cdr list1) list2))
	    (t nil)))

(defun set>= (list1 list2)
	(set<= list2 list1))

(defun set> (list1 list2)
	(set< list2 list1))

(defun set= (list1 list2)
	(and (set<= list1 list2) (set<= list2 list1)))

(defun set/= (list1 list2)
	(not (set= list1 list2)))

;; relation-like list


;; print list
(defun print-list (&optional lst (start 0))
	;; example: (print-list '(i love you))
    (do ((x lst (cdr x)) (k start (1+ k)))
        ((null x) nil)
        (format t "~d: ~a~%" k (car x))))

;; string
(defun str+ (&rest strings) (apply #'concatenate (cons 'string strings)))

(defun str* (&optional (s "") (n 1))
	(cond ((<= n 0) "")
	((equal n 1) s)
	(t (str+ s (str* s (1- n))))))


;(show (setitem '(1 2) 1 1))