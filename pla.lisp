(defun self-list* (w x)
  (reduce #'+ 
	  (mapcar #'* w x)))

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
    	     (when (/= (self-sign (self-list* w (car data))) (cdr data))
    	       (setf w (self-list+ w (mapcar (lambda (x_i)
    					       (* x_i (cdr data))) (car data)))
    		     flag 1)))))
    w))
