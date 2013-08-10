(in-package :md)

(defun guess-problem (problem)
  "Tries to guess a solution for given problem, parameters:
     problem - an instance of problem-interface"
  (handler-bind ((problem-solved (lambda (x)
                                   (declare (ignore x))
                                   (return-from guess-problem t))))
    (choose-run-and-return
     (choose-do
       term <- (construct-program-1 (problem-size problem)
                                    (encode-set (problem-operators problem)))
       (let ((vals (problem-examples problem)))
         (if (bv-check-values term (mapcar #'car vals)
                              (mapcar #'cdr vals))
             (unless (problem-guess problem (bv-program-to-string term))
               (fail))
             (fail)))))))
