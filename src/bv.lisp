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
  (declare ((unsigned-byte 64) bytes accum)
           (fixnum iter))
  (if (= iter 8)
      accum
      (bv-fold (ash bytes -8)
               (funcall func (list top-var (logand bytes 255) accum))
               top-var
               func
               (1+ iter))))

(defun fix-64 (v)
  (declare ((signed-byte 66) v))
  (logand v (1- (ash 1 64))))

(defun make-shift (shift)
  (lambda (v)
    (declare ((unsigned-byte 64) v)
             (fixnum shift))
      (fix-64 (ash v shift))))

(defvar *bv-un-ops* (pairlis '(not shl1 shr1 shr4 shr16)
                             (list (lambda (v)
                                     (declare ((unsigned-byte 64) v))
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
                                      (declare ((unsigned-byte 64) v1 v2))
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
  (let ((pr (bv-fold-constants pr)))
    (bv-compile-expr (bv-lambda-body pr)
                     (list (cons (bv-lambda-var pr)
                                 #'first)))))

(defun bv-run (compiled-prog val)
  (funcall compiled-prog (list val)))

(defun bv-read-program (str)
  (read-from-string str))

(defun bv-program-to-string (pr)
  (string-downcase (format nil "~A" pr)))

(defun bv-run-values (pr vals)
  (let ((cpr (bv-compile-program pr)))
    (mapcar (lambda (v)
              (bv-run cpr v))
            vals)))

(defun bv-check-values (pr inputs results &key (fail-func (lambda (x) 
							    (declare (ignore x))
							    nil)))
  (let ((cpr (bv-compile-program pr)))
     (mapcar (lambda (i r)
               (declare ((unsigned-byte 64) i r))
	       (let ((result (= (bv-run cpr i)
				r)))
		 (unless result
		   (unless (funcall fail-func (cons i r))
		     (return-from bv-check-values nil)))
		 result))
	     inputs
	     results)))

(declaim (ftype (function (t) (unsigned-byte 64)) bv-size))

(defun bv-size (term)
  (if (bv-atom? term)
      1
      (case (bv-operator term)
        ((not shl1 shr1 shr16 shr4) (1+ (bv-size (bv-arg1 term))))
        ((and or xor plus) (+ 1 (bv-size (bv-arg1 term))
                              (bv-size (bv-arg2 term))))
        (if0 (+ 1 (bv-size (bv-arg1 term))
                (bv-size (bv-arg2 term))
                (bv-size (bv-arg3 term))))
        (fold (+ 2 (bv-size (bv-arg1 term))
                 (bv-size (bv-arg2 term))
                 (bv-size (bv-fold-lambda-body term))))
        (lambda (1+ (bv-size (bv-lambda-body term)))))))

(defun bv-op (term)
  (optima:ematch term
    ((optima:guard x (bv-atom? x))
     (op-set-empty))
    ((list 'if0 e0 e1 e2)
     (op-set 'if0
	     (op-union (bv-op e0)
		       (op-union (bv-op e1)
				 (bv-op e2)))))
    ((list op e1)
     (op-set op
	     (bv-op e1)))
    ((list op e1 e2)
     (op-set op
	     (op-union (bv-op e1)
		       (bv-op e2))))
    ((list 'fold e0 e1 (list 'lambda (list _ _) e2))
     (op-set 'fold
	     (op-union (bv-op e0)
		       (op-union (bv-op e1)
				 (bv-op e2)))))))

(defun bv-operators (term)
  (optima:ematch term
    ((list 'lambda (list x) (list 'fold x1 0 (list 'lambda (list x2 _) e)))
     (unless (and (eq x x1)
                  (eq x1 x2))
       (optima:fail))
     (op-set 'tfold
	     (bv-op e)))
    ((list 'lambda (list _) e)
     (bv-op e))))

(defun bv-fold-constants (term)
  (macrolet ((with-number (op &body body)
                          `(let ((e1 (bv-fold-constants e1)))
                             (if (numberp e1)
                               (progn ,@body)
                               (list ,op e1))))
             (with-number2 (op &rest variants)
                           (let ((e1-body (find-if (lambda (x) (eq (car x) :e1)) variants))
                                 (both-body (find-if (lambda (x) (eq (car x) :both)) variants))
                                 (default-body (find-if (lambda (x) (eq (car x) :default)) variants)))
                             `(let ((e1 (bv-fold-constants e1))
                                    (e2 (bv-fold-constants e2)))
                                (cond
                                  ((and (numberp e1)
                                        (numberp e2)
                                        ,@both-body))
                                  ((numberp e1) ,@e1-body)
                                  ((numberp e2)
                                   (let ((e1 e2)
                                         (e2 e1))
                                     ,@e1-body))
                                  (t ,(if default-body
                                        `(progn ,@default-body)
                                        `(list ,op e1 e2))))))))
    (optima:ematch term
                   ((list 'if0 e1 e2 e3)
                    (if (numberp e1)
                      (if (= 0 e1)
                        (bv-fold-constants e2)
                        (bv-fold-constants e3))
                      (list 'if0
                            (bv-fold-constants e1)
                            (bv-fold-constants e2)
                            (bv-fold-constants e3))))
                   ((list 'not   e1) (with-number 'not   (fix-64 (lognot e1))))  
                   ((list 'shl1  e1) (with-number 'shl1  (fix-64 (ash e1 1))))  
                   ((list 'shr1  e1) (with-number 'shr1  (ash e1 -1)))
                   ((list 'shr4  e1) (with-number 'shr4  (ash e1 -4)))
                   ((list 'shr16 e1) (with-number 'shr16 (ash e1 -16)))
                   ((list 'and e1 e2)
                    (with-number2
                      'and
                      (:e1 (cond ((= e1 0) 0)
                                 ((= e1 #xFFFFFFFFFFFFFFFF) e2)
                                 (t (list 'and e1 e2))))
                      (:both (logand e1 e2))))
                   ((list 'or e1 e2)
                    (with-number2
                      'or
                      (:e1 (cond ((= e1 0) e2)
                                 ((= e1 #xFFFFFFFFFFFFFFFF) e1)
                                 (t (list 'or e1 e2))))
                      (:both (logior e1 e2))))
                   ((list 'xor e1 e2)
                    (with-number2
                      'xor
                      (:e1 (cond ((= e1 0) e2)
                                 ((= e1 #xFFFFFFFFFFFFFFFF) e2)
                                 (t (list 'xor e1 e2))))
                      (:both (logxor e1 e2))
                      (:default
                        (if (equal e1 e2)
                          0 
                          (list 'xor e1 e2)))))
                   ((list 'plus e1 e2)
                    (with-number2
                      'plus
                      (:e1 (cond ((= e1 0) e2)
                                 (t (list 'plus e1 e2))))
                      (:both (fix-64 (+ e1 e2)))))
                   ((list 'lambda x e1)
                    (let ((e1 (bv-fold-constants e1)))
                      (list 'lambda x e1)))
                   ((list 'fold e1 e2 e3)
                     (let ((e1 (bv-fold-constants e1))
                           (e2 (bv-fold-constants e2))
                           (e3 (bv-fold-constants e3)))
                       (list 'fold e1 e2 e3)))
                   (x x))))
        
         
    
