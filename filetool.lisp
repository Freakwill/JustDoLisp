(defun fop (filename &optional (op #'read-line))
	(let ((fp (open filename)))
		(funcall op fp)
		(close fp)))


(fop "mylist.lisp")