;; oil wells (problem G)

(defun intersect? (a b)
  (let ((ax0 (elt a 0))
        (ax1 (elt a 1))
        (ay (elt a 2))
        (bx0 (elt b 0))
        (bx1 (elt b 1))
        (by (elt b 2)))
    (and (> (abs (- ay by))
            0)
         (or (and (>= bx1 ax0)
                  (<= bx1 ax1))
             (and (>= bx0 ax0)
                  (<= bx0 ax1))))))

(defun make-path-from (well wells)
  (cond ((null wells)
         '())
        ((intersect? well
                     (car wells))
         (cons (car wells)
               (make-path-from (car wells)
                               (cdr wells))))
        (t (make-path-from well
                           (cdr wells)))))

(defun path-weight (path)
  (reduce (lambda (a b)
            (let ((bx0 (elt b 0))
                  (bx1 (elt b 1)))
              (+ a
                 (- bx1 bx0))))
          path
          :initial-value 0))

(defun get-path-weight (wells)
  (let* ((partial-path (make-path-from (car wells)
                                       (cdr wells)))
         (full-path (cons (car wells)
                          partial-path)))
    (abs (path-weight full-path))))

(defun best-path-weight (wells)
  (cond ((null wells) 0)
        (t (max (get-path-weight wells)
                (best-path-weight (cdr wells))))))

;;; tests

(print (best-path-weight '((100 180 20)
                           (30 60 30)
                           (70 110 40)
                           (10 40 50)
                           (0 80 70))))
;; => 200

(print (best-path-weight '((50 60 10)
                           (-42 -42 20)
                           (25 0 10))))
;; => 25
