(defun self-random-list (num &optional (size 200))
  (let ((up-size (1+ size))
        (half-size (/ size 2)))
    (loop for no from 1 to num
       collect (/ (- (random up-size) half-size) half-size))))

(defun self-sign (x)
  (cond ((< x 0) -1)
        ((> x 0) 1)
        (t -1)))

(defun self-noise ()
  (if (< (random 5) 1)
      -1
      1))

(defun self-datas (num)
  (mapcar (lambda (x)
            (cons x (* (self-sign x) (self-noise))))
          (self-random-list num)))

(defun self-stump (x s theta)
  (* s (self-sign (- x theta))))
