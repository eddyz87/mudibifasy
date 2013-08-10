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
       :initarg :id)))

(defmethod problem-guess ((p a-real-problem) str)
  (let* ((result (guess (problem-id p) str))
         (status (get-json-field "status" result))
         (vals (get-json-field "values" result)))
    (cond
      ((string= "win" status) (progn (signal 'problem-solved) t))
      ((string= "mismatch")
         (progn
           (if (and (listp vals)
                    (cddr vals))
             (push (cons (first vals) (second vals)) (dummy-examples p))
             (warn "Malformed values returned by guess request: ~A" result))
           nil))
      (t (progn
           (warn "Unexpected status=~A in problem-guess, response is ~A" status result)
           nil)))))

(defun make-a-real-problem (id size ops &optional (number-of-examples 10000))
  (let* ((examples (make-hash-table :test #'eq))
         (examples-as-pairs nil)
         (iters-left number-of-examples))
    ;; collect all examples
    (loop while (< 0 iters-left) do
          (let ((xs-size (if (< iters-left 256) iters-left 256))
                (xs))
            ;; collect random x
            (loop while (< 0 xs-size) do
                  (let ((x (random *64-bit-max*)))
                    (unless (gethash x examples)
                      (setf (gethash x examples) t)
                      (push x xs)
                      (decf xs-size))))
            ;; request f x
            (setf examples-as-pairs
                  (append
                    (mapcar #'cons xs (request-eval xs :id id))
                    examples-as-pairs))
            (decf iters-left 256)))
    (make-instance
      'a-real-problem
      :id id
      :size size
      :examples examples-as-pairs
      :ops (??? ops))))
  
  
 
