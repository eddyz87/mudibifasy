(in-package :md)

(defparameter *paths* nil)
(defconstant failsym '@)

#|
(defmacro choose (&rest choices)
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push #'(lambda () ,c) *paths*))
                   (reverse (cdr choices)))
         , (car choices))
      '(fail)))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(defun cb (fn choices)
  (if choices
      (progn
        (if (cdr choices)
            (push #'(lambda () (cb fn (cdr choices)))
                  *paths*))
        (funcall fn (car choices)))
      (fail)))

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))
|#

(defun choose-return (x)
  (lambda (cont fail-cont)
    (funcall cont x fail-cont)))

(defun choose-bind (m f)
  (lambda (cont fail-cont)
    (funcall m 
             (lambda (v fail-cont-2)
               (funcall (funcall f v)
                        cont
                        fail-cont-2))
             fail-cont)))

(defun fail ()
  (lambda (cont fail-cont)
    (declare (ignore cont))
    (funcall fail-cont)))

(defun fail-if (fail?)
  (if fail?
    (fail)
    (choose-return nil)))

(defvar *choose-randomize* nil)

(defun choose-one (lst)
  (lambda (cont fail-cont)
    (if (null lst)
        (funcall fail-cont)
        (let ((first-val (if *choose-randomize*
                             (nth (random (length lst)) lst)
                             (car lst))))
          (funcall cont first-val
                   (lambda ()
                     (funcall (choose-one (if *choose-randomize*
                                              (remove first-val lst :test #'eq)
                                              (cdr lst)))
                              cont
                              fail-cont)))))))

(defmacro choose-do (&body clauses)
  (labels ((%tr (cls)
             (let ((s (gensym)))
               (cond ((null (cdr cls))
                      (car cls))
                     ((eq (second cls)
                          '<-)
                      `(choose-bind ,(caddr cls)
                           (lambda (,(car cls))
                             ,(%tr (cdddr cls)))))
                     (t 
                      `(choose-bind ,(car cls)
                           (lambda (,s)
                             ,(%tr (cdr cls)))))))))
    (%tr clauses)))

(defun choose-run (ch ok-cont fail-cont)
  (funcall ch ok-cont fail-cont))

(defun choose-test (sum)
  (choose-run 
   (choose-do 
     v1 <- (choose-merge
            (choose-one (loop for i from 1 to 10 collect i))
            (choose-one (loop for i from 100 to 110 collect i)))
     v2 <- (choose-one (loop for i from 1 to 10 collect i))
     (if (= (+ v1 v2)
            sum)
         (choose-return (cons v1 v2))
         (fail)))
   (lambda (v fail-cont)
     (declare (ignore fail-cont))
     (return-from choose-test v))
   (lambda ()
     (error "Not found"))))

(defun choose-plus (m1 m2)
  (lambda (cont fail-cont)
    (funcall m1 cont
             (lambda ()
               (funcall m2
                        cont
                        fail-cont)))))

(defun choose-merge (&rest args)
  (reduce #'choose-plus
          (cdr args)
          :initial-value (car args)))

(defvar *unary-op-set* (encode-set '(not shl1 shr1 shr4 shr16)))
(defvar *binary-op-set* (encode-set '(and or xor plus)))

(defun construct-term (size vars op-set &optional (current-op))
;  (format t "construct-term size ~A vars ~A op-set ~A~%" size vars op-set)
  (if (= size 1)
    (choose-one (append (if (and current-op (member current-op '(shr1 shr4 shr16 shl1) :test #'eq))
                          (if (eq current-op 'shl1)
                            (list 1)
                            nil)
                          (list 0 1))
                        vars))
      (choose-merge
        (choose-do
          op <- (choose-one (decode-set
                              (op-intersection op-set
                                               *unary-op-set*)))
          sub-term <- (construct-term (1- size) vars op-set op)
          (choose-return (list op sub-term)))
        (if (<= size 2)
          (fail)
          (choose-do
            op <- (choose-one (decode-set 
                                (op-intersection op-set
                                                 *binary-op-set*)))
            sz <- (choose-one (loop for i from 1 to (truncate (/ (1- size) 2))
                                    ;;sz <- (choose-one (loop for i from 1 to (- size 2)
                                    collect i))
            sub-term1 <- (construct-term sz vars op-set)
            (fail-if (and (member op '(and or plus xor) :test #'eq) (equal (bv-fold-constants sub-term1) 0)))
            sub-term2 <- (construct-term (- size sz 1) vars op-set)
            (choose-return
              (list op sub-term1
                    sub-term2))))
        (if (or (<= size 3)
                (not (op-test 'if0 op-set)))
          (fail)
          (choose-do
            sz <- (choose-one (loop for i from 2 to (- size 2)
                                    collect i))
            sz1 <- (choose-one (loop for i from 1 to (- sz 1)
                                     collect i))
            sub-term-c <- (construct-term (- size sz 1) vars op-set)
            (fail-if (member (bv-fold-constants sub-term-c) '(0 1) :test #'equal))
            sub-term-t <- (construct-term sz1 vars op-set)
            sub-term-f <- (construct-term (- sz sz1) vars op-set)
            (choose-return
              (list 'if0 
                    sub-term-c
                    sub-term-t
                    sub-term-f))))
        (if (or (<= size 4)
                (not (op-test 'fold op-set)))
          (fail)
          (let ((no-fold-set (op-unset 'fold op-set)))
            (choose-do
              sz <- (choose-one (loop for i from 2 to (- size 3)
                                      collect i))
              sz1 <- (choose-one (loop for i from 1 to (- sz 1)
                                       collect i))
              sub-term-1 <- (construct-term sz1 vars no-fold-set)
              sub-term-2 <- (construct-term (- sz sz1) vars no-fold-set)
              v1 <- (choose-one (cons 'x vars))
              v2 <- (choose-one (cons 'z vars))
              sub-term-b <- (construct-term (- size sz 2) 
                                            (append vars (list v1 v2))
                                            no-fold-set)
              (choose-return
                (list 'fold
                      sub-term-1
                      sub-term-2
                      (list 'lambda (list v1 v2)
                            sub-term-b)))))))))

(defun construct-program-1-bonus (size op-set)
  ;; - (lambda + if0 + and + 1)
  (let ((body-size (- size 4)))
    (choose-do
      expr-sizes <- (choose-do
		      e1 <- (choose-one (loop for i from 1 to (- body-size 2) collect i))
		      e2 <- (choose-one (loop for i from 1 to (- body-size 2) collect i))
		      e3 <- (choose-one (loop for i from 1 to (- body-size 2) collect i))
		      
		      (if (eq (+ e1 e2 e3) body-size)
			  (choose-return (list e1 e2 e3))
			  (fail)))
      term1 <- (construct-term (car expr-sizes) '(x) op-set)
      term2 <- (construct-term (cadr expr-sizes) '(x) op-set)
      term3 <- (construct-term (caddr expr-sizes) '(x) op-set)

      (let ((progr `(lambda (x)
		      (if0 (and ,term1 1) ,term2 ,term3))))
	(let ((ops (bv-operators progr)))
	  (if (eq ops op-set)
	      (choose-return progr)
	      (fail)))))))

(defun construct-program-1 (size op-set)
  (choose-do
    term <- (if (op-test 'tfold op-set)
                (choose-do
                  t1 <- (construct-term (- size 5) '(x y) (op-unset 'tfold op-set))
                  (choose-return `(fold x 0 (lambda (x y) ,t1))))
                (construct-term (1- size) '(x) op-set))
    (let ((progr `(lambda (x)
                    ,term)))
      (let ((ops (bv-operators progr)))
        ;; (format t "Test : ~A~%"
        ;;         (string-downcase (format nil "~A"
        ;;                                  progr)))
        (if t;(eq ops op-set)
            (choose-return progr)
            (fail))))))

(defun guess-program-1 (size op-set vals)
  (choose-do
    term <- (construct-program-1 size op-set)
    (if (bv-check-values term (mapcar #'car vals)
                         (mapcar #'cdr vals))
        (choose-return term)
        (fail))))

(defun choose-run-and-return (choose-comp)
  (choose-run
   choose-comp
   (lambda (v fail-cont)
     (declare (ignore fail-cont))
     (return-from choose-run-and-return v))
   (lambda ()
     (return-from choose-run-and-return nil))))

(defun construct-program (size op-set)
  (choose-run-and-return
   (if (member 'bonus op-set)
       (construct-program-1-bonus size (encode-set (remove 'bonus op-set)))
       (construct-program-1 size (encode-set op-set)))))

(defun guess-program (size op-set vals)
  (choose-run-and-return
   (guess-program-1 size (encode-set op-set) vals)))
