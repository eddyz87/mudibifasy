(in-package :md)

(defun guess-problem (problem)
  "Tries to guess a solution for given problem, parameters:
  problem - an instance of problem-interface"
  (handler-bind ((problem-solved (lambda (x)
                                   (declare (ignore x))
                                   (return-from guess-problem t))))
            (choose-run-and-return
                          (choose-do
                            size <- (choose-one (loop for i from 8 to (problem-size problem) collecting i))
                            term <- (construct-program-1 size
                                                         (encode-set (problem-operators problem)))
                            (let ((vals (problem-examples problem)))
                              (if (bv-check-values term (mapcar #'car vals)
                                                   (mapcar #'cdr vals))
                                (unless (problem-guess problem (bv-program-to-string term))
                                  (fail))
                                (fail)))))
            ))

(defun guess-problem-silly (problem)
  "Tries to guess a solution for given problem, parameters:
     problem - an instance of problem-interface"
  (handler-bind ((problem-solved (lambda (x)
                                   (declare (ignore x))
                                   (return-from guess-problem-silly t))))
    (choose-run-and-return
     (choose-do
       term <- (construct-program-1 (problem-size problem)
                                    (encode-set (problem-operators problem)))
       (let (;(term (problem-info-body info))
             (vals (problem-examples problem)))
         (if (bv-check-values term (mapcar #'car vals)
                              (mapcar #'cdr vals))
           (progn 
             (warn "Term before constant folding: ~A~%" term)
             (unless (problem-guess problem (bv-program-to-string (bv-fold-constants term)))
               (fail)))
             (fail)))))))
