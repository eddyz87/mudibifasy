(in-package :md)

(defun bv-compile-term-to-function (term)
  (let* ((lisp-form (bv-turn-to-lisp term)))
    ;;(warn "Lsp form: ~A" lisp-form)
    (eval `(progn
             ;;(declaim (optimize speed))
             ,lisp-form))))

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64) t) (unsigned-byte 64)) do-bv-fold))

(defun do-bv-fold (bytes accum func)
  ;;(declare (optimize speed))
  (declare ((unsigned-byte 64) bytes accum))
  (let ((byte1 (logand bytes #xFF))
        (byte2 (logand (ash bytes -8) #xFF))
        (byte3 (logand (ash bytes -16) #xFF))
        (byte4 (logand (ash bytes -24) #xFF))
        (byte5 (logand (ash bytes -32) #xFF))
        (byte6 (logand (ash bytes -40) #xFF))
        (byte7 (logand (ash bytes -48) #xFF))
        (byte8 (logand (ash bytes -56) #xFF)))
    (funcall func byte8
      (funcall func byte7
        (funcall func byte6
          (funcall func byte5
            (funcall func byte4
              (funcall func byte3
                (funcall func byte2
                  (funcall func byte1 accum))))))))))

(defun bv-turn-to-lisp (term)
  (macrolet ((%compile1 (&body body)
                          `(let ((e1 (bv-turn-to-lisp e1)))
                             ,@body))
             (%compile2 (&body body)
                          `(let ((e1 (bv-turn-to-lisp e1))
                                 (e2 (bv-turn-to-lisp e2)))
                             ,@body))
             (%compile3 (&body body)
                          `(let ((e1 (bv-turn-to-lisp e1))
                                 (e2 (bv-turn-to-lisp e2))
                                 (e3 (bv-turn-to-lisp e3)))
                             ,@body)))
    (optima:ematch term
                   ((list 'if0 e1 e2 e3) (%compile3 `(if (= 0 ,e1) ,e2 ,e3)))
                   ((list 'not e1) (%compile1 `(fix-64 (lognot ,e1))))
                   ((list 'shl1 e1) (%compile1 `(fix-64 (ash ,e1 1))))
                   ((list 'shr1 e1) (%compile1 `(ash ,e1 -1)))
                   ((list 'shr4 e1) (%compile1 `(ash ,e1 -4)))
                   ((list 'shr16 e1) (%compile1 `(ash ,e1 -16)))
                   ((list 'and e1 e2) (%compile2 `(logand ,e1 ,e2)))
                   ((list 'or  e1 e2) (%compile2 `(logior ,e1 ,e2)))
                   ((list 'xor e1 e2) (%compile2 `(logxor ,e1 ,e2)))
                   ((list 'plus e1 e2) (%compile2 `(fix-64 (+ ,e1 ,e2))))
                   ((list 'lambda x e1) (%compile1 `(lambda ,x ,e1)))
                   ((list 'fold e1 e2 e3) (%compile3 `(do-bv-fold ,e1 ,e2 ,e3)))
                   (x x))))

(defun bv-test-compiler (term total-iter)
  (let ((func (bv-compile-term-to-function term))
        (func1 (bv-compile-program term)))
    (loop for i from 0 to total-iter do
          (let ((val (random *64-bit-max*)))
            (unless (= (funcall func val)
                       (funcall func1 (list val)))
              (error "Functions do not match on ~A" val))))))

(defun bv-bench-compiler (term total-iter eval-iter)
  (format t "--- eval version ---~%")
  (time
    (loop for i from 0 to total-iter do
          (let ((func (bv-compile-term-to-function term)))
            (loop for j from 0 to eval-iter do
                  (funcall func (random *64-bit-max*))))))
  (format t "--- lambda version ---~%")
  (time
    (loop for i from 0 to total-iter do
          (let ((func (bv-compile-program1 term)))
            (loop for j from 0 to eval-iter do
                  (funcall func (list (random *64-bit-max*))))))))

(defun bench1 (iter eval-iter)
  (bv-bench-compiler '(lambda (x_85597)
                        (fold x_85597 0 (lambda (x_85597 x_85598)
                                          (shl1 (or (if0 (and (shr16 (not (xor
                                            (shl1 (shr4 (shr1 (plus (shr16 (or (xor x_85597 (not x_85597)) 0))
                                            (shr16 0))))) 1))) 0) x_85597 x_85597) x_85598)))))
                     iter eval-iter))
