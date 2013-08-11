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

(defparameter *bv-un-ops* (pairlis '(not shl1 shr1 shr4 shr16)
                                   (list (lambda (v)
                                           (declare ((unsigned-byte 64) v))
                                           (fix-64 (lognot v)))
                                         (make-shift 1)
                                         (make-shift -1)
                                         (make-shift -4)
                                         (make-shift -16))))

(defparameter *bv-bin-ops* (pairlis '(and or xor plus)
                                    (list #'logand
                                          #'logior
                                          #'logxor
                                          (lambda (v1 v2)
                                            (declare ((unsigned-byte 64) v1 v2))
                                            (fix-64 (+ v1 v2))))))

(defstruct unknown
  (bits 0)
  (mask 0))

(defun to-unknown (v)
  (if (typep v 'unknown)
      v
      (make-unknown :bits v
                    :mask (1- *64-bit-max*))))

(defmacro with-unknowns (vars &body body)
  `(let ,(mapcar (lambda (v)
                   `(,v (to-unknown ,v)))
                 vars)
       ,@body))

(defun make-unk-shift (shift)
  (lambda (v)
    (unk-shift v shift)))

(defun unk-shift (v shift)
  (declare (fixnum shift))
  (if (typep v 'unknown)
      (make-unknown :bits (fix-64 (ash (unknown-bits v) shift))
                    :mask (logior (fix-64 (ash (unknown-mask v) shift))
                                  (if (> shift 0)
                                      1
                                      (logxor (1- *64-bit-max*)
                                              (1- (ash 1 (+ 64 shift)))))))
      (fix-64 (ash v shift))))

(defun unk-not (v)
  (with-unknowns (v)
    (make-unknown :bits (fix-64 (lognot (unknown-bits v)))
                  :mask (unknown-mask v))))

(defun wrap-unknown (func)
  (lambda (v1 v2)
    (with-unknowns (v1 v2)
      (if (and (= 0 (lognot (unknown-mask v1)))
               (= 0 (lognot (unknown-mask v2))))
          (make-unknown :mask (unknown-mask v1)
                        :bits (fix-64 (funcall func 
                                               (unknown-bits v1) 
                                               (unknown-bits v2))))
          (make-unknown)))))

(defun unk-logand (v1 v2)
  (with-unknowns (v1 v2)
    (let* ((zeros1 (logior (lognot (unknown-mask v1))
                           (unknown-bits v1)))
           (zeros2 (logior (lognot (unknown-mask v2))
                           (unknown-bits v2)))
           (zeros (logand zeros1 zeros2))
           (known (logand (logand (unknown-mask v1)
                                  (unknown-mask v2))
                          (logand (unknown-bits v1)
                                  (unknown-bits v2)))))
      (make-unknown :mask (fix-64
                           (logior (logand (unknown-mask v1)
                                           (unknown-mask v2))
                                   (lognot zeros)))
                    :bits (logand known zeros)))))

(defun unk-logior (v1 v2)
  (with-unknowns (v1 v2)
    (let ((res (unk-logand
                (make-unknown :mask (unknown-mask v1)
                              :bits (lognot (unknown-bits v1)))
                (make-unknown :mask (unknown-mask v2)
                              :bits (lognot (unknown-bits v2))))))
      (make-unknown :mask (unknown-mask res)
                    :bits (fix-64 (lognot (unknown-bits res)))))))

(defun unk-logxor (v1 v2)
  (with-unknowns (v1 v2)
    (make-unknown :mask (logand (unknown-mask v1)
                                (unknown-mask v2))
                  :bits (logxor (unknown-bits v1)
                                (unknown-bits v2)))))

(defparameter *bv-un-ops-unk* (pairlis '(not shl1 shr1 shr4 shr16)
                                 (list #'unk-not
                                       (make-unk-shift 1)
                                       (make-unk-shift -1)
                                       (make-unk-shift -4)
                                       (make-unk-shift -16))))

(defparameter *bv-bin-ops-unk* (pairlis '(and or xor plus)
                                  (list #'unk-logand
                                        #'unk-logior
                                        #'unk-logxor
                                        (wrap-unknown (lambda (v1 v2)
                                                        (declare ((unsigned-byte 64) v1 v2))
                                                        (fix-64 (+ v1 v2)))))))

(defvar *bv-if-op* 'if)
(defvar *bv-fold-op* 'fold)

(defun unknown-if (e0 e1 e2)
  (with-unknowns (e0 e1 e2)
    (if (= 0 (lognot (unknown-mask e0)))
        (if (= 0 (unknown-bits e0))
            e1
            e2)
        (if (/= 0 (logand (unknown-mask e0)
                          (unknown-bits e0)))
            e2
            (make-unknown :mask (logand
                                 (logand (unknown-mask e1)
                                         (unknown-mask e2))
                                 (fix-64 (lognot 
                                          (logxor (unknown-bits e1)
                                                  (unknown-bits e2)))))
                          :bits (logand (unknown-bits e1)
                                        (unknown-bits e2)))))))

(defun unknown-fold (bytes accum top-var func &optional (iter 0))
  (declare (fixnum iter))
  (if (= iter 8)
      accum
      (unknown-fold 
       (unk-shift bytes -8)
       (funcall func (list top-var (unk-logand bytes 255) accum))
       top-var
       func
       (1+ iter))))

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
               (if (eq *bv-if-op* 'if)
                   (lambda (vars)
                     (if (= (funcall arg1 vars) 0) 
                         (funcall arg2 vars)
                         (funcall arg3 vars)))
                   (let ((if-fun *bv-if-op*))
                     (lambda (vars)
                       (funcall if-fun
                                (funcall arg1 vars)
                                (funcall arg2 vars)
                                (funcall arg3 vars)))))))
           (%make-fold ()
             (let* ((arg1 (bv-compile-expr (bv-arg1 term) env))
                    (arg2 (bv-compile-expr (bv-arg2 term) env))
                    (elem-v (bv-fold-lambda-elem-var term))
                    (accum-v (bv-fold-lambda-accum-var term))
                    (body (bv-compile-expr (bv-fold-lambda-body term)
                                           (cons (cons elem-v #'second)
                                                 (cons (cons accum-v #'third)
                                                       env)))))
               (if (eq *bv-fold-op* 'fold)
                   (lambda (vars)
                     (bv-fold (funcall arg1 vars)
                              (funcall arg2 vars)
                              (first vars)
                              body))
                   (let ((fold-fun *bv-fold-op*))
                     (lambda (vars)
                       (funcall fold-fun
                                (funcall arg1 vars)
                                (funcall arg2 vars)
                                (first vars)
                                body)))))))
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

(defun bv-compile-partial-program (pr)
  (let ((*bv-un-ops* *bv-un-ops-unk*)
        (*bv-bin-ops* *bv-bin-ops-unk*)
        (*bv-if-op* #'unknown-if)
        (*bv-fold-op* #'unknown-fold))
    (bv-compile-expr ;;(bv-fold-constants 
                      (bv-lambda-body pr);;)
                     (list (cons (bv-lambda-var pr)
                                 #'first)))))

(defun bv-compile-program1 (pr)
  (bv-compile-expr (bv-lambda-body pr)
                   (list (cons (bv-lambda-var pr)
                               #'first))))

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

(defun bv-compiled-run-values (cpr vals)
  (mapcar (lambda (v)
            (bv-run cpr v))
          vals))

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
        (lambda (1+ (bv-size (bv-lambda-body term))))
        (otherwise 0))))

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

(defun bv-has-unknown (term)
  (optima:ematch term
    ((optima:guard x (bv-atom? x))
     (typep x 'unknown))
    ((list 'if0 e0 e1 e2)
     (or (bv-has-unknown e0)
         (bv-has-unknown e1)
         (bv-has-unknown e2)))
    ((list 'lambda (list _) e0)
     (bv-has-unknown e0))
    ((list op e1)
     (declare (ignore op))
     (bv-has-unknown e1))
    ((list op e1 e2)
     (declare (ignore op))
     (or (bv-has-unknown e1)
         (bv-has-unknown e2)))
    ((list 'fold e0 e1 (list 'lambda (list _ _) e2))
     (or (bv-has-unknown e0)
         (bv-has-unknown e1)
         (bv-has-unknown e2)))))

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
                    (let ((ne1 (bv-fold-constants e1)))
                    (if (numberp ne1)
                      (if (= 0 ne1)
                        (bv-fold-constants e2)
                        (bv-fold-constants e3))
                      (let ((ne2 (bv-fold-constants e2))
                            (ne3 (bv-fold-constants e3)))
                      (if (equal e2 e3)
                        (bv-fold-constants e2)
                        (list 'if0 ne1 ne2 ne3))))))
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
        
         
    
