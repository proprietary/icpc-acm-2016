;; tests

(solution +i1+) ;; => 4
(solution +i2+) ;; => 8
(solution +i3+) ;; => 1

(defvar +i1+ '((0 10 0)
	       (10 0 1)
	       (12 8 1)
	       (5 5 0)
	       (11 2 1)
	       (11 3 0)))

(defvar +i2+ '((6 1 1)
	       (0 2 0)
	       (2 1 1)
	       (6 1 1)
	       (8 2 0)
	       (4 4 0)
	       (4 0 0)
	       (2 3 1)
	       (6 1 0)
	       (6 3 1)))

(defvar +i3+ '((5 7 0)
	       (3 4 0)
	       (5 7 0)
	       (5 7 1)
	       (9 4 0)))

;; code

(defun sorting-pred (x y S* T*)
  "S* and T* must be in scope"
  (let ((a1 (car x))
	(b1 (cadr x))
	(a2 (car y))
	(b2 (cadr y)))
    (> (+ (* S* a1)
	  (* T* b1))
       (+ (* S* a2)
	  (* T* b2)))))

(defun first-is (lst what &optional &key (test #'equal))
  "Index of first element in LST that is WHAT"
  (cond ((null lst) nil) ;; failure
	((funcall test (car lst) what) 0)
	(t (1+ (first-is (cdr lst) what :test test)))))

(defun last-is (lst what &optional &key (test #'equal))
  "Index of last element in LST that is WHAT"
  (- (length lst)
     (first-is (reverse lst) what :test test)))

(defun cluster-size (lst)
  (let* ((test #'(lambda (x what)
		   (eql (nth 2 x) what)))
	 (j (first-is lst 1 :test test))
	 (k (last-is lst 1 :test test)))
    (1+ (- k j))))

(defun try-clustering (S* T* lst)
  (cluster-size
   (stable-sort (copy-list lst)
		#'(lambda (x y)
		    (sorting-pred x y S* T*)))))

(defun try-clustering-lst (S* T* lst)
  (stable-sort (copy-list lst)
	       #'(lambda (x y)
		   (sorting-pred x y S* T*))))

(defun brute-force-min-cluster-size (lst max &optional &key (S*-guess 1) (T*-guess 1))
  (let ((best (list (list 'cluster-size most-positive-fixnum)
		    (list 'S* 0)
		    (list 'T* 0))))
    (loop for S* from S*-guess to max do
	 (loop for T* from T*-guess to max
	    with res = (1- (try-clustering S* T* lst))
	    when (< res
		    (cadr (nth 0 best)))
	    do (progn (setf (cadr (nth 0 best)) res)
		      (setf (cadr (nth 1 best)) S*)
		      (setf (cadr (nth 2 best)) T*))))
    best))

(defun solution (lst &optional &key (n 10))
  (cadr (assoc 'cluster-size
	       (brute-force-min-cluster-size lst n))))
