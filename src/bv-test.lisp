(in-package :md)

(defun group (num list)
  (labels ((%inner (curr-count curr-list rest-list acc)
	     (if rest-list
		 (if (plusp curr-count)
		     (%inner (1- curr-count) 
			     (cons (car rest-list) curr-list)
			     (cdr rest-list)
			     acc)
		     (%inner num nil rest-list (cons (reverse curr-list) acc)))
		 (cons (reverse curr-list) acc))))
    (reverse (%inner num nil list nil))))
		  
(defun perform-test (program values)
  (labels ((%fail-func (result)
	     (let ((actual (car result))
		   (expected (cdr result)))
	       (warn "Actual = ~A, but expected = ~A" actual expected)
	       t)))
    (let ((term-program (bv-read-program program))
	  (all-vals (group 256 values)))
      (mapcar (lambda (val-group)
		(let ((expected-vals (request-eval val-group :program program)))
		  (bv-check-values 
		   term-program 
		   val-group 
		   expected-vals
		   :fail-func #'%fail-func)))
	    all-vals))))

(defun gen-test-case (num)
  (let ((generated-vals (make-hash-table :test #'eq))
	(xs-size num)
	(xs nil))
    (loop while (plusp xs-size) do
	 (let ((x (random *64-bit-max*)))
	   (unless (gethash x generated-vals)
	     (setf (gethash x generated-vals) t)
	     (push x xs)
	     (decf xs-size)))
	 finally (return xs))))


				  