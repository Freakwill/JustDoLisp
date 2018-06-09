(load "D:\\clisp\\mylisp\\mylist.lisp")
;; basic functions
(defun path-add (node path)
	if (null path) nil (cons node path))

;; search


(defun dfs (start goal next &optional dead)
	(let ((f #'(lambda (x) (dfs x goal next (cons s dead)))) (next-states (set-difference (next start) '(start))))
	    (if (equal start goal) (if (member start dead) () (list start))
	    (mapcar #'(lambda (x) (cons start x)) (mapcar #'f next-starts)))
	)
)

(defun dfs1 (start goal next &optional dead)
	"return the first path finded"
	(let ((f #'(lambda (x) (dfs x goal next (cons start dead)))) (next-states (set-difference (next start) '(start)))
	    (if (equal start goal) (if (member start dead) () (list start))
	    (path-add start (find-if #'not-null (mapcar #'f next-starts)))))))

(defun dfs2 (start goal next &optional dead)
	"return the shortest path"
	(let ((f #'(lambda (x) (dfs x goal next (cons s dead)))) (next-states (set-difference (next start) '(start))))
	    (if (equal start goal) (if (member start dead) () (list start))
	    (cons start (car (mapcar #'f next-starts))))
	)
)

