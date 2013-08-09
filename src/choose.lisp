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

(defun choose-one (lst)
  (labels ((%cont-with-list (l cont fail-cont)
             (if (null l)
                 (funcall fail-cont)
                 (%cont-with-list (cdr l) cont fail-cont))))
    (lambda (cont fail-cont)
      (if (null lst)
          (funcall fail-cont)
          (funcall cont (car lst)
                   (lambda ()
                     (funcall (choose-one (cdr lst))
                              cont
                              fail-cont)))))))

(defmacro choose-do (&body clauses)
  (labels ((%tr (cls)
             (let ((s (gensym)))
               (cond ((null (cdr cls))
                      (car cls))
                     ((eq (second (car cls))
                          '<-)
                      `(choose-bind ,(caddr (car cls))
                           (lambda (,(car (car cls)))
                             ,(%tr (cdr cls)))))
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
     (v1 <- (choose-merge
             (choose-one (loop for i from 1 to 10 collect i))
             (choose-one (loop for i from 100 to 110 collect i))))
     (v2 <- (choose-one (loop for i from 1 to 10 collect i)))
     (if (= (+ v1 v2)
            sum)
         (choose-return (cons v1 v2))
         (fail)))
   (lambda (v fail-cont)
     (declare (ignore fail-cont))
     (return-from choose-test v))
   (lambda ()
     (error "Not found"))))

(defun choose-merge (m1 m2)
  (lambda (cont fail-cont)
    (funcall m1 cont
             (lambda ()
               (funcall m2
                        cont
                        fail-cont)))))


(defun construct-term (size vars)
  (if (= size 1)
      (choose-one (append (list 0 1)
                          vars))
      (choose-merge
       (choose-do
         (op <- (choose-one '(not shl1 shr1 shr4 shr16)))
         (sub-term <- (construct-term (1- size) vars))
         (choose-return (list op sub-term)))
       (if (<= size 2)
           (fail)
           (choose-do
             (op <- (choose-one '(and or xor plus)))
             (sz <- (choose-one (loop for i from 1 to (- size 2)
                                   collect i)))
             (sub-term1 <- (construct-term sz vars))
             (sub-term2 <- (construct-term (- size sz 1) vars))
             (choose-return
              (list op sub-term1
                    sub-term2)))))))

(defun construct-program-1 (size op-set)
  (choose-do
    (term <- (construct-term (1- size) '(x)))
    (let ((progr (list 'lambda (list 'x)
                       term)))
      (let ((ops (bv-operators progr)))
        (if (and (subsetp ops op-set)
                 (subsetp op-set ops))
            (choose-return progr)
            (fail))))))

(defun construct-program (size op-set)
  (choose-run
   (construct-program-1 size op-set)
   (lambda (v fail-cont)
     (declare (ignore fail-cont))
     (return-from construct-program v))
   (lambda ()
     (return-from construct-program nil))))