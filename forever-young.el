;; This buffer is for notes you don't want to save, and for Lisp evaluation. ;; If you want to create a file, visit that file with C-x C-f,;; then enter the text in that file's own buffer.

(require 'cl-lib)

(defvar +table+ '(0 1 2 3 4 5 6 7 8 9
                    a b c d e f g h i j k l m n
                    o p q r s t u v w x y z))

(elt +table+ 10)

(defun n-in-m-base (n m)
  (cond ((zerop n)
         '())
        (t (append (n-in-m-base (/ n m) m)
                   (list (elt +table+
                              (mod n m)))))))

(defun non-dec-digit? (x)
  (not (integerp x)))

(defun list-to-decimal (lst)
  (string-to-number (mapconcat #'number-to-string
                               lst
                               "")))

(defun highest-dec-digit-base (y l guess)
  (let ((try (n-in-m-base y guess)))
    (if (and (cl-notany #'non-dec-digit?
                        try)
             (>= (list-to-decimal try)
                 l))
        guess
      (highest-dec-digit-base y l (1- guess)))))

(defun forever-young (y l)
  (highest-dec-digit-base y l y))

;;;; tests
(forever-young 32 20) ;; => 16
(let ((max-specpdl-size (expt 2 60))
      (max-lisp-eval-depth (expt 2 60)))
  (forever-young 2016 100)) ;; => 42
