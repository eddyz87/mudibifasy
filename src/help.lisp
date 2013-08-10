(in-package :md)

(defparameter *interface-functions*
  (list 'auth
        'problems
        'request-eval
        'guess
        'train
        'status
        'make-a-real-problem
        'guess-problem
        ))

(defun help ()
  (mapcar (lambda (fsym) (format nil "~A: ~A" (string-downcase (symbol-name fsym)) (documentation fsym 'function)))
          *interface-functions*))
