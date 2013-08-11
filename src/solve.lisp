(in-package :md)

(defun guess-problem (problem)
  "Tries to guess a solution for given problem, parameters:
     problem - an instance of problem-interface"
  (handler-bind ((problem-solved (lambda (x)
                                   (declare (ignore x))
                                   (return-from guess-problem t))))
    (choose-run-and-return
     (choose-do
       size <- (choose-one (loop for i from 6 to (problem-size problem) collecting i))
       term <- (let ((ex-vals (subseq (problem-examples problem) 0 4)))
                 (if (member 'bonus (problem-operators problem))
                     (construct-program-1-bonus size
                                                (encode-set (remove 'bonus (problem-operators problem)))
                                                (mapcar #'car ex-vals)
                                                (mapcar #'cdr ex-vals))
                     (construct-program-1 size
                                          (encode-set (problem-operators problem))
                                          (mapcar #'car ex-vals)
                                          (mapcar #'cdr ex-vals))))
       (let ((vals (problem-examples problem)))
         (if (bv-check-values term (mapcar #'car vals)
                              (mapcar #'cdr vals))
             (unless (problem-guess problem (bv-program-to-string term))
               (fail))
             (fail)))))))

(defun choose-prog-test (size op-set)
  (let ((num 0))
    (choose-run-and-return
     (choose-do
       term <- (construct-program-1 size
                                    (encode-set op-set)
                                    nil
                                    nil)
       (choose-return (progn (incf num)
                             (format t "Constructed : ~A~%" (bv-program-to-string term))))
       (fail)))
    num))
