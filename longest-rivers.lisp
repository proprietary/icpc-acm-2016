(defun add (x y)
  (+ x y))

(add 5 34)
(add 8 19)

(defun river-length (river confluences)
  (let* ((confluence-idx (cadr river))
         (confluence-length (if (eq 0 confluence-idx) 0
                                (cadr (elt confluences (1- confluence-idx)))))
         (river-basic-length (elt river 2))
         (river-name (car river)))
    (cons river-name
          (+ river-basic-length
             confluence-length))))

(defun add-ranks (sorted-rivers &optional (n 1) (last-length 0))
  (cond ((null sorted-rivers)
         '())
        (t (let* ((curr-length (cdar sorted-rivers))
                  (this-n (if (eq last-length curr-length)
                              (1- n)
                              n))
                  (new-n (1+ this-n))
                  (fst (car sorted-rivers)))
             (cons (cons this-n fst)
                   (add-ranks (cdr sorted-rivers)
                              new-n
                              curr-length))))))

(defun rank-rivers (rivers confluences)
  (let ((ranked (add-ranks (sort (mapcar (lambda (x)
                                           (river-length x confluences))
                                         rivers)
                                 #'>
                                 :key #'cdr))))
    (mapcar (lambda (x)
              (let ((river-name (car x)))
                (cons river-name
                      (car (find river-name
                                 ranked
                                 :key #'cadr)))))
            rivers)))

;; test

(defvar +result+ (rank-rivers '((PaSak 0 513)
                                (Nan 2 675)
                                (Yom 2 700)
                                (Wang 1 335)
                                (Ping 1 305)
                                (ThaChin 0 765))
                              '((0 353)
                                (0 65))))

;; => ((PASAK . 5) (NAN . 2) (YOM . 1) (WANG . 3) (PING . 4) (THACHIN . 1))

(princ +result+)
