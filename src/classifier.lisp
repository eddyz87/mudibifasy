(in-package :md)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defvar *classifiers* nil)
  (defvar *promoters* nil))

(defmacro def-classifier (cl-tp name vars &body body)
  ;;(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *classifiers*
          (cons (cons name (cons cl-tp (compile nil `(lambda ,vars ,@body))))
                (remove name *classifiers* :test #'eq :key #'car)))
    t);;)

(defmacro def-promoter (name vars &body body)
  (setf *promoters*
        (cons (cons name (compile nil `(lambda ,vars ,@body)))
              (remove name *promoters* :test #'eq :key #'car)))
  t)

(defmacro def-point-classifier (name (v v1) func &body body)
  (let ((s1 (gensym)))
    `(def-classifier :point ,name (,v ,s1)
                     (let ((,v1 (gethash (,func (car ,v)) ,s1)))
                       (if ,v1
                           (progn ,@body)
                           :not-found)))))

(defmacro def-two-point-classifier (name (v1 v2 r) func &body body)
  (let ((s1 (gensym)))
    `(def-classifier :two-point ,name (,v1 ,v2 ,s1)
                     (let ((,r (gethash (,func (car ,v1) (car ,v2)) ,s1)))
                       (if ,r
                           (progn ,@body)
                           :not-found)))))

(defmacro def-point-promoter (name (val) &body body)
  (let ((s1 (gensym)))
    `(def-promoter ,name (,s1)
       (mapcar (lambda (,val)
                 ,@body)
               ,s1))))

(defmacro def-two-point-promoter (name (v1 v2) &body body)
  (let ((s1 (gensym)))
    `(def-promoter ,name (,s1)
       (mapcan (lambda (,v1)
                 (mapcar (lambda (,v2)
                           ,@body)
                         ,s1))
               ,s1))))

(def-classifier :linear monotonic (l-points point r-points)
  (if (and l-points
           r-points)
      (eq (<= (- (cdr point)
                 (cdr (first l-points)))
              0)
          (<= (- (cdr (first r-points))
                 (cdr point))
             0))
      t))

(def-point-classifier not-distr (v r1)
  (lambda (v)
    (fix-64 (lognot v)))
  (= (cdr v) 
     (fix-64 (lognot r1))))

(def-point-promoter not-add (v) 
  (fix-64 (lognot v)))

(def-point-classifier not-eq (v r1)
  (lambda (v)
    (fix-64 (lognot v)))
  (= (cdr v) 
     r1))

(def-two-point-classifier and-distr (v1 v2 r1)
  (lambda (v1 v2)
    (logand v1 v2))
  (= (logand (cdr v1) (cdr v2))
     r1))

(def-two-point-classifier or-distr (v1 v2 r1)
  (lambda (v1 v2)
    (logior v1 v2))
  (= (logior (cdr v1) (cdr v2))
     r1))

(def-two-point-classifier xor-distr (v1 v2 r1)
  (lambda (v1 v2)
    (logxor v1 v2))
  (= (logxor (cdr v1) (cdr v2))
     r1))

(def-two-point-promoter or-add (v1 v2) 
  (logior v1 v2))

(def-two-point-promoter and-add (v1 v2) 
  (logand v1 v2))

(def-two-point-promoter xor-add (v1 v2) 
  (logxor v1 v2))

(defvar *classifier-threshold* 0.4)

(defun check-linear-classifier (vals func)
  (let* ((num-points (length vals))
         (l-points nil)
         (r-points vals)
         (num-classified 0)
         (point nil))
    (loop for i from 1 to num-points
       do (progn
            (setf point (pop r-points))
            (when (funcall func l-points point r-points)
              (incf num-classified))
            (push point l-points)))
    (let ((ratio (/ num-classified num-points)))
      (if (> ratio *classifier-threshold*)
          ratio
          nil))))

(defun check-point-classifier (vals func)
  (let ((point-hash (make-hash-table))
        (num-found 0)
        (num-classified 0))
    (mapc (lambda (v)
            (setf (gethash (car v) point-hash)
                  (cdr v)))
          vals)
    (mapc (lambda (v)
            (let ((res (funcall func v point-hash)))
              (unless (eq res :not-found)
                (incf num-found)
                (when res
                  (incf num-classified)))))
          vals)
    (if (= num-found 0)
        nil
        (let ((ratio (/ num-classified num-found)))
          (if (> ratio *classifier-threshold*)
              ratio
              nil)))))

(defun check-two-point-classifier (vals func)
  (let ((point-hash (make-hash-table))
        (num-found 0)
        (num-classified 0))
    (mapc (lambda (v)
            (setf (gethash (car v) point-hash)
                  (cdr v)))
          vals)
    (mapc (lambda (v1)
            (mapc (lambda (v2)
                    (let ((res (funcall func v1 v2 point-hash)))
                      (unless (eq res :not-found)
                        (incf num-found)
                        (when res
                          (incf num-classified)))))
                  vals))
          vals)
    (if (= num-found 0)
        nil
        (let ((ratio (/ num-classified num-found)))
          (if (> ratio *classifier-threshold*)
              ratio
              nil)))))

(defun check-classifiers (vals)
  (let ((sorted (sort vals #'< :key #'car)))
    (remove nil
            (mapcar (lambda (cls)
                      (let ((res (case (cadr cls)
                                   (:linear (check-linear-classifier sorted (cddr cls)))
                                   (:point (check-point-classifier sorted (cddr cls)))
                                   (:two-point (check-two-point-classifier sorted (cddr cls))))))
                        (if res
                            (cons (car cls) res)
                            nil)))
                    *classifiers*))))

(defun take-list-part (lst ratio)
  (let ((num (truncate (* (- 1.0 ratio) (length lst)))))
    (butlast lst num)))

(defun promote-vals (vals &key (add-ratio 0.2))
  (let* ((vs1 (take-list-part vals add-ratio))
         (adds (mapcan (lambda (pr)
                         (funcall (cdr pr) vs1))
                       *promoters*)))
    (append vals adds)))