;; tests

(solution +drives+) ;; => 1
(solution +drives2+) ;; => 5

(defvar +drives+ '((6 6)
		   (1 7)
		   (3 5)
		   (3 5)))

(defvar +drives2+ '((2 2)
		    (3 3)
		    (5 1)
		    (5 10)))

;; code

(defun sort-drives (drives free-space)
  (if (null drives) '()
      (append (sort-drives (remove-if-not #'(lambda (x)
					      (and (<= (car x) free-space)
						   (< (abs (- (car x) free-space))
						      (abs (- (caar drives) free-space)))))
					  (cdr drives))
			   free-space)
	      (list (car drives))
	      (sort-drives (remove-if-not #'(lambda (x)
					      (or (> (car x) free-space)
						  (>= (abs (- (car x) free-space))
						      (abs (- (caar drives) free-space)))))
					  (cdr drives))
			   free-space))))

(defun try-extra-hdd (drives free-space)
;; TODO memoize this for better perf.
  "initialize FREE-SPACE w/ the extra hdd space to try first"
  (let* ((sorted-drives (sort-drives drives free-space))
	 (head (pop sorted-drives)))
	 ;;(excess-space (+ (- free-space (caar head))
			  ;;(cadr head))))
    (cond ((null drives) 0)
	  ((not (eql (car head) free-space)) 0)
	  (t (+ (cadr head)
		(try-extra-hdd sorted-drives
			       (cadr head)))))))

(defun max-of-list (fn lst)
  (cond ((null (cdr lst)) (funcall fn (car lst)))
	((> (funcall fn (car lst))
	    (max-of-list fn (cdr lst)))
	 (car lst))
	(t (max-of-list fn (cdr lst)))))

(defun max-of (fn next input end)
  "NEXT gets the next INPUT. END is a function testing end condition."
  (let ((next-input (funcall next input)))
    (cond ((funcall end next-input) (funcall fn input))
	  ((> (funcall fn input)
	      (max-of fn next next-input end))
	   input)
	  (t (max-of fn next next-input end)))))

(defun dumb-brute-force (drives num)
  (let ((res (max-of #'(lambda (x) (try-extra-hdd drives x))
		     #'1+
		     1
		     #'(lambda (x) (eql x num)))))
    (if (not (zerop res))
	res
	(dumb-brute-force drives (* 2 num)))))

(defun solution (drives)
  (dumb-brute-force drives 10))
