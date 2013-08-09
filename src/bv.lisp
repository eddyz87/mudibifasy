(in-package :md)

(defun bv-atom? (term)
  (atom term))

(defun bv-var? (term)
  (symbolp term))

(defun bv-operator (term)
  (car term))

(defun bv-arg1 (term)
  (second term))

(defun bv-arg2 (term)
  (third term))

(defun bv-arg3 (term)
  (fourth term))

(defun bv-lambda-var (term)
  (car (second term)))

(defun bv-lambda-body (term)
  (third term))

(defun bv-fold-lambda-elem-var (term)
  (first (second (fourth term))))

(defun bv-fold-lambda-accum-var (term)
  (second (second (fourth term))))

(defun bv-fold-lambda-body (term)
  (third (fourth term)))

(defun bv-lookup-func (term env)
  (cdr (assoc term env :test 'eq)))

(defun bv-fold (bytes accum top-var func &optional (iter 0))
  (if (= iter 9)
      accum
      (bv-fold (ash bytes -8)
               (funcall func (list top-var (logand bytes 255) accum))
               top-var
               func
               (1+ iter))))

(defun fix-64 (v)
  (logand v (ash 1 64)))

(defun make-shift (shift)
  (lambda (v)
    (fix-64 (ash v shift))))

(defvar *bv-un-ops* (pairlis '(not shl1 shr1 shr4 shr16)
                             (list (lambda (v)
                                     (fix-64 (lognot v)))
                                   (make-shift 1)
                                   (make-shift -1)
                                   (make-shift -4)
                                   (make-shift -16))))

(defvar *bv-bin-ops* (pairlis '(and or xor plus)
                              (list #'logand
                                    #'logior
                                    #'logxor
                                    (lambda (v1 v2)
                                      (fix-64 (+ v1 v2))))))

(defun bv-compile-expr (term env)
  "Translate \BV expr to lambda ((top-var fold-elem fold-var)).
env is assoc list of symbol -> function (first | second | third)"
  (labels ((%make-un-op ()
             (let ((func (cdr (assoc (bv-operator term) *bv-un-ops*)))
                   (arg (bv-compile-expr (bv-arg1 term) env)))
               (lambda (vars)
                 (funcall func (funcall arg vars)))))
           (%make-bin-op ()
             (let ((func (cdr (assoc (bv-operator term) *bv-bin-ops*)))
                   (arg1 (bv-compile-expr (bv-arg1 term) env))
                   (arg2 (bv-compile-expr (bv-arg2 term) env)))
               (lambda (vars)
                 (funcall func 
                          (funcall arg1 vars)
                          (funcall arg2 vars)))))
           (%make-if-op ()
             (let ((arg1 (bv-compile-expr (bv-arg1 term) env))
                   (arg2 (bv-compile-expr (bv-arg2 term) env))
                   (arg3 (bv-compile-expr (bv-arg3 term) env)))
               (lambda (vars)
                 (if (= (funcall arg1 vars) 0) 
                     (funcall arg2 vars)
                     (funcall arg3 vars)))))
           (%make-fold ()
             (let* ((arg1 (bv-compile-expr (bv-arg1 term) env))
                    (arg2 (bv-compile-expr (bv-arg2 term) env))
                    (elem-v (bv-fold-lambda-elem-var term))
                    (accum-v (bv-fold-lambda-accum-var term))
                    (body (bv-compile-expr (bv-fold-lambda-body term)
                                           (cons (cons elem-v #'second)
                                                 (cons (cons accum-v #'third)
                                                       env)))))
               (lambda (vars)
                 (bv-fold (funcall arg1 vars) 
                          (funcall arg2 vars) 
                          (first vars)
                          body)))))
    (if (bv-atom? term)
        (if (bv-var? term)
            (bv-lookup-func term env)
            (lambda (vars)
              (declare (ignore vars))
              term))
        (case (bv-operator term)
          ((not shl1 shr1 shr16 shr4) (%make-un-op))
          ((and or xor plus) (%make-bin-op))
          (if0 (%make-if-op))
          (fold (%make-fold))))))

(defun bv-compile-program (pr)
  (bv-compile-expr (bv-lambda-body pr)
                   (list (cons (bv-lambda-var pr)
                               #'first))))

(defun bv-run (compiled-prog val)
  (funcall compiled-prog (list val)))

(defun bv-read-program (str)
  (read-from-string str))

(defun bv-run-values (pr vals)
  (let ((cpr (bv-compile-program pr)))
    (mapcar (lambda (v)
              (bv-run cpr v))
            vals)))

(defun bv-check-values (pr inputs results)
  (let ((cpr (bv-compile-program pr)))
    (every (lambda (i r)
             (= (bv-run cpr i)
                r))
           inputs
           results)))

