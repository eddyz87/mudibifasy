(in-package :md)

(defvar *interface-functions*
  (list 'auth
        'problems))

(defun help ()
  (mapcar (lambda (fsym) (format nil "~A: ~A" (string-downcase (symbol-name fsym)) (documentation fsym 'function)))
          *interface-functions*))
