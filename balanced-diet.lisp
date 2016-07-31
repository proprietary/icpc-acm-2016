;; TESTS
(balanced-diet '(6 5)
	       '(2 1 6 3 5 3)
	       '(1 2 5 3 5))
;; => 1

(balanced-diet '(6 4)
	       '(2 1 6 3 5 3)
	       '(1 2 5 3))
;; prints "forever"


;; CODE

(defun balanced-diet (M-K A B)
  (let ((M (car M-K)))
    (- (length (dfs-longest-path M A B '()))
       (length B))))

(defun inv-cdr (lst)
  (subseq lst 0 (- (length lst) 1)))

(defun dfs-longest-path (M A B states &optional (first t))
  ;;(format t "~A~%" states)
  (cond ((> (length states)
	    1000) ;; what to do about this without going over time limit?
	    ; most-positive-fixnum)
	 (progn (format t "forever")
		nil))
	((and (null first) (null states)) B)
	;((null (last states))
	; (car states))
	(t (let ((possible-nexts (which-types-balance? M A B)))
	     (if possible-nexts
		 (dfs-longest-path
		  M A
		  (append B (list (car possible-nexts)))
		  (append (cdr possible-nexts)
			  states)
		  nil)
		 (dfs-longest-path
		  M A
		  (append (inv-cdr B)
			  (list (car states)))
		  (cdr states)
		  nil))))))

(defun most-extra-and-balanced (M A B i)
  ;; ignore this one
  (if (= i most-positive-fixnum)
      (format t "forever")
      (let ((B* (any-types-balance? M A B)))
	(format t "~A ~A ~A : ~A~%" M A B B*)
	(if (null B*)
	    i
	    (most-extra-and-balanced M A B* (1+ i))))))

(defun which-types-balance? (M A B)
  "For an appended type to B in range [1, M], which ones balance?"
  (if (= M 0)
      '()
      (let* ((B* (append B (list M)))
	     (Ss (S A B*))
	     (n (apply #'+ Ss))
	     (i (1- (length A))))
	(if (balanced? n A Ss i)
	    (cons M
		  (which-types-balance? (1- M) A B))
	    (which-types-balance? (1- M) A B)))))

(defun any-types-balance? (M A B)
  ;; ignore this one
  "For any type (up to M) added to B, does it still balance?"
  (if (= M 0)
      t
      (let* ((B* (append B (list M)))
	     (Ss (S A B*))
	     (n (apply #'+ Ss))
	     (i (1- (length A))))
	(if (or (balanced? n A Ss i)
		(all-types-balance? (1- M) A B))
	    B*
	    nil))))

(defun Fi (A i)
  (/ (elt A i)
     (reduce #'+ A)))

(defun Fis (A)
  ;; ignore this one
  (defun Fis-iter (i acc)
    (if (= i (length A)) acc
	(Fis-iter (1+ i) (append (list (Fi A i)) acc))))
  (iter 0 '()))

(defun S (A B)
  "List of sweets consumed where list index = sweet type index"
  (defun S-iter (A B i)
    (if (null A) '()
	(cons (count-if (lambda (x)
			  (= x (1+ i)))
			B)
	      (S-iter (cdr A)
		      B
		      (1+ i)))))
  (S-iter A B 0))

(defun balanced? (n A S i)
  "Determine if a combination is balanced"
  ;; n is total number of sweets Danny ate
  (if (< i 0)
      t
      (and (< (1- (* n (Fi A i)))
	      (elt S i))
	   (< (elt S i)
	      (1+ (* n (Fi A i))))
	   (balanced? n A S (1- i)))))
