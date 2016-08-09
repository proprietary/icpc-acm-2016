;; tests

(k-value '(1)) ;; => "no quotation"
(k-value '(22)) ;; => 4
(k-value '(2 1 1 1 3)) ;; => 2

;; code

(defconstant *failure* "no quotation")

(defun k-value (A)
  (cond ((equal '(1) A) *failure*)
	((eql 1 (length A))
	 (1- (max-k (list (quot (car A) 2)))))
	(t (max-k A))))

(defun max-k (A)
  (let* ((head (iff-multiple #'car A))
	 (end (iff-multiple #'(lambda (x) (car (last x))) A))
	 (inner (if (and (zerop head)
			 (zerop end))
		    (list (car A))
		    (get-inner A))))
    (quot (+ (abs (- head end))
	     (apply #'+ inner))
	  2)))

(defun quot (x y)
  (values (floor x y)))

(defun iff-multiple (fn lst)
  "IFF list.length > 1, apply FN to LST. Else, 0."
  (if (> (length lst) 1)
      (funcall fn lst)
      0))

(defun get-inner (lst)
  (cond ((> (length lst) 2)
	 (subseq lst 1 (1- (length lst))))
	(t '())))

(defun count-inner-quotes (inner-list)
  (if (null inner-list)
      0
      (apply #'+ inner-list)))
