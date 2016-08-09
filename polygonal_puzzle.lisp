(defun verticies-equal? (vertex1 vertex2)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list vertex1 vertex2)
    (and (eql x1 x2)
	 (eql y1 y2))))

(defun lines-overlap? (line1 line2)
  (destructuring-bind (((x00 y00) (x01 y01))
		       ((x10 y10) (x11 y11)))
      (list line1 line2)
    (or (or (and (>= x00 x10) ;; either line1 is in line2...
		 (<= x01 x11)
		 (eql y00 y10)
		 (eql y01 y11))
	    (and (>= y00 y10)
		 (<= y01 y11)
		 (eql x00 x10)
		 (eql x01 x11)))
	(or (and (>= x10 x00) ;; or line2 is in line1
		 (<= x11 x01)
		 (eql y00 y10)
		 (eql y01 y11))
	    (and (>= y10 y00)
		 (<= y11 y01)
		 (eql x00 x10)
		 (eql x01 x11))))))

(defun lines-touch? (line1 line2)
  "Are two lines formed by a pair of verticies colinear?"
  (destructuring-bind ((x00 y00) (x01 y01)
		       (x10 y10) (x11 y11))
      (list line1 line2)
    (or (and (verticies-equal? (car line1) (car line2))
	     (verticies-equal? (cdr line1) (cdr line2)))
	(lines-overlap? line1 line2))))

(defun line-slope (vertex1 vertex2)
  (destructuring-bind ((x1 y1) (x2 y2))
      (list vertex1 vertex2)
    (let ((den (- x2 x1))
	  (num (- y2 y1)))
      (if (or (zerop den) (zerop num)) 0
	  (/ num den)))))

(defun circular (lst)
  "Makes LST a circular list"
  (let ((cpy (copy-list lst)))
    (setf (cdr (last cpy)) cpy)
    cpy))

(defun slopes-of-shape (shape)
  "Turns a list of verticies SHAPE into a list of slopes"
  (let ((circular-vertices (circular shape)))
    (loop repeat (length shape)
       for vertex1 = (pop circular-vertices)
       for vertex2 = (pop circular-vertices)
       collect (line-slope vertex1 vertex2))))

(defun square (x)
  (* x x))

(defun cart-distance (vertex1 vertex2)
  "Distance between two vertices"
  (destructuring-bind ((x1 y1) (x2 y2))
      (list vertex1 vertex2)
    (sqrt
     (+ (square (- x2 x1))
	(square (- y2 y1))))))

(defun min-of-list (f lst)
  "Find the minimum in LST according to F"
  (cond ((null (cdr lst)) (funcall f (car lst)))
	((< (funcall f (car lst))
	    (min-of-list f (cdr lst)))
	 (car lst))
	(t (cadr lst))))

(defun vertices-intersection (vertex1 vertex2)
  (min-of-list #'(lambda (xs) (apply #'+ (mapcar #'abs xs)))
	       (list vertex1 vertex2)))

(defun shapes-intersection (shape1 shape2)
  "Intersectional verticies between two shapes (lists of vertices)"
  (let ((circular-shape1 (circular shape1))
	(circular-shape2 (circular shape2)))
    (loop repeat (min (length shape1) (length shape2))
       for vertex1 = (pop circular-shape1)
       for vertex2 = (pop circular-shape2)
       collect (vertices-intersection vertex1 vertex2))))

(defun make-even-list (lst)
  "Makes a list have an even number of elements, taking from the first if odd"
  (if (evenp (length lst))
      lst
      (append lst (list (car lst)))))

(defun visit-vertices (f combiner null-value last lst)
  "Visits every vertex pair in shape. Call with CAR as LAST and CDR as LST."
  (cond ((null lst) null-value)
	(t (funcall combiner
		    (funcall f last (car lst))
		    (visit-vertices f combiner null-value
				    (car lst) (cdr lst))))))

(defun colinear-distance (shape1 shape2)
  "Returns the total distance that two shapes touch"
  (visit-vertices #'(lambda (v1 v2)
		      (let* ((shape2-v1 (pop shape2))
			     (shape2-v2 (car shape2))
			     (slope1 (line-slope v1 v2))
			     (slope2 (line-slope shape2-v1
						 shape2-v2)))
			(if (or (or (eql (* -1 slope1)
					  slope2)
				     (eql (* -1 slope2)
					  slope1))
				 (zerop (acute-angle
					 (list v1 v2)
					 (list shape2-v1 shape2-v2))))
			    (prog1 (min (cart-distance v1 v2)
				 (cart-distance shape2-v1 shape2-v2))
			      (format t "~a ~a ~a~%" (list v1 v2)
				      (list shape2-v1 shape2-v2)
				      (min (cart-distance v1 v2) (cart-distance shape2-v1 shape2-v2))))
			    0)))
		  #'+ 0 (car shape1) (cdr shape1)))

(defun acute-angle (line1 line2)
  (destructuring-bind (((x00 y00) (x01 y01))
		       ((x10 y10) (x11 y11)))
      (list line1 line2)
    (let ((m2 (line-slope (list x10 y10)
			  (list x11 y11)))
	  (m1 (line-slope (list x00 y00)
			    (list x01 y01))))
      (atan (abs (/ (- m2 m1)
		    (+ 1 (abs (* m1 m2)))))))))

(defun shift-list (lst n)
  "Moves list cicularly by N"
  )

(defun max-common-boundary (shape1 shape2)
  )

(defvar +i1+ '((0 0)
	       (0 10)
	       (10 10)
	       (15 15)
	       (24 6)
	       (24 10)
	       (30 10)
	       (30 0)))

(defvar +i2+ '((-5 0)
	       (-5 10)
	       (10 10)
	       (15 5)
	       (20 10)
	       (35 10)
	       (35 0)))
