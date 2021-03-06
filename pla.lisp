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

(defun self-read-string (str)
  (let ((beg-pos 0)
        (nums '()))
    (do* ((num (read-from-string str nil 'eof :start beg-pos)
               (read-from-string str nil 'eof :start beg-pos)))
         ((eql num 'eof))
      (multiple-value-bind (my-num my-pos)
          (read-from-string str nil 'eof :start beg-pos)
        (setf beg-pos my-pos)
        (push my-num nums)))
    (values (reverse (cdr nums)) (car nums))))

(defun self-read-data (fname)
  (let ((dbs '()))
    (with-open-file (str fname :direction :input)
      (do ((line (read-line str nil 'eof)
                 (read-line str nil 'eof)))
          ((eql line 'eof))
        (multiple-value-bind (x y)
            (self-read-string line)
          (push (cons (append '(1) x) y) dbs))))
    (reverse dbs)))
