(defun self-list* (x y)
  (reduce #'+
	  (mapcar #'* x y)))

(defun self-list+ (x y)
  (mapcar #'+ x y))

(defun self-sign (x)
  (cond ((< x 0) -1)
	((> x 0) 1)
	(t 0)))

(defun self-pla (dbs)
  (let* ((len (list-length (caar dbs)))
	 (w (make-list len :initial-element 0))
	 (flag 1))
    (loop while (= flag 1) do
    	 (progn
    	   (setf flag 0)
    	   (dolist (data dbs)
             (let ((x (car data))
                   (y (cdr data)))
               (when (/= (self-sign (self-list* w x)) y)
                 (setf w (self-list+ w (mapcar (lambda (x_i)
                                                 (* x_i y)) x))
                       flag 1))))))
    w))
