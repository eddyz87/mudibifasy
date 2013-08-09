(in-package :md)

(defvar *interface-functions*
  (list 'auth
        'problems))

(defun help ()
  (mapcar (lambda (fsym) (documentation fsym 'function))
          *interface-functions*))
