(in-package :md)

(defclass problem-interface () ())

(define-condition problem-solved () ())

(defgeneric problem-operators (p)
  (:documentation "returns the list of operators as symbols"))
(defgeneric problem-size (p)
  (:documentation "returns the size of the problem as integer"))
(defgeneric problem-examples (p)
  (:documentation "returns the list of pairs (x . f x)"))
(defgeneric problem-guess (p program-string)
  (:documentation "if guess is succesfull signals problem-solved, nil - otherwise"))

(defclass dummy-problem (problem-interface)
  ((ops :accessor dummy-ops
        :initarg :ops)
   (size :accessor dummy-size
         :initarg :size)
   (program :accessor dummy-program
            :initarg :program)
   (examples :accessor dummy-examples
             :initarg :examples)))

(defmethod problem-operators ((p dummy-problem)) (dummy-ops p))
(defmethod problem-size ((p dummy-problem)) (dummy-size p))
(defmethod problem-examples ((p dummy-problem)) (dummy-examples p))
(defmethod problem-guess ((p dummy-problem) str)
  (warn "Trying solution: '~A'~%" str)
  (when (string= str (dummy-program p))
    (warn "Solution found: '~A'~%" str)
    (signal (make-instance 'problem-solved))))

(defclass a-real-problem (dummy-problem)
  ((id :accessor problem-id
       :initarg :id)
   (known-values :accessor problem-known-values
                 :initarg :known-values)))

(defvar *guess-mismatch-known-values-step* 32)

(defmethod problem-guess ((p a-real-problem) str)
  (let* ((result (guess (problem-id p) str))
         (status (get-json-field "status" result))
         (vals (decode-arguments (get-json-field "values" result))))
    (cond
      ((string= "win" status)
       (warn "Solution found: ~A" str)
       (signal 'problem-solved)
       t)
      ((string= "mismatch" status)
       (warn "Solution mismatch: ~A vals ~A" str vals)
       (if (and (listp vals)
                (cddr vals))
         (progn
           (push (cons (first vals) (second vals)) (dummy-examples p))
           (setf (dummy-examples p)
                 (append
                   (request-more-examples
                     (problem-id p)
                     (problem-known-values p)
                     *guess-mismatch-known-values-step*)
                   (dummy-examples p))))
         (warn "Malformed values returned by guess request: ~A" result))
       nil)
      (t (warn "Unexpected status=~A in problem-guess, response is ~A" status result)
         nil))))

(defparameter *64-bit-max* (ash 1 64))

(defun request-more-examples (id known-values number-of-examples)
  (let* ((examples-as-pairs nil)
         (iters-left number-of-examples))
    ;; collect all examples
    (loop while (< 0 iters-left) do
          (let ((xs-size (if (<= iters-left 256) iters-left 256))
                (xs))
            ;; collect random x
            (loop while (< 0 xs-size) do
                  (let ((x (random *64-bit-max*)))
                    (unless (gethash x known-values)
                      (setf (gethash x known-values) t)
                      (push x xs)
                      (decf xs-size))))
            ;; request f x
            (setf examples-as-pairs
                  (append
                    (mapcar #'cons xs (request-eval xs :id id))
                    examples-as-pairs))
            (decf iters-left 256)))
    examples-as-pairs))

(defun make-a-real-problem (info &optional (number-of-examples *guess-mismatch-known-values-step*))
  "create a real problem interface for the given problem, parameters:
     info - problem-info"
  (warn "Gona try to solve: ~A" info)
  (let* ((examples (make-hash-table :test #'eq)))
    (make-instance
      'a-real-problem
      :id (problem-info-id info)
      :size (problem-info-size info)
      :examples (request-more-examples (problem-info-id info) examples number-of-examples)
      :known-values examples
      :ops (problem-info-operators info))))
