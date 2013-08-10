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
  (when (string= str (dummy-program p))
    (signal (make-instance 'problem-solved))))
