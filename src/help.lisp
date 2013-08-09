(in-package :md)

(defparameter *interface-functions*
  (list 'auth
        'problems
        'request-eval
        'guess
        'train
        'status
        ))

(defun help ()
  (mapcar (lambda (fsym) (format nil "~A: ~A" (string-downcase (symbol-name fsym)) (documentation fsym 'function)))
          *interface-functions*))
