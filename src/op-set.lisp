(in-package :md)

(defvar *operators* '(if0 fold not shl1 shr1 shr4 shr16 and or xor plus tfold))
(loop for op in *operators* 
      for val = 1 then (ash val 1) do
     (setf (get op 'set-val) val))

(defun op-set-empty ()
  "Empty op set"
  0)

(defun op-set (op set)
  "Adds operator to the set"
  (logior (get op 'set-val) set))

(defun op-unset (op set)
  "Removes operator from the set"
  (logand (lognot (get op 'set-val)) set))

(defun op-test (op set)
  "Tests if operator does occur in set"
  (not (zerop (logand (get op 'set-val) set))))

(defun op-union (set-1 set-2)
  "Computes union of two sets"
  (logior set-1 set-2))

(defun op-intersection (set-1 set-2)
  "Computes intersection of two sets"
  (logand set-1 set-2))

(defun encode-set (ops)
  "Converts list of operators int set"
  (loop for op in ops
        with set = 0 do
       (setf set (op-set op set))
       finally (return set)))

(defun decode-set (set)
  "Converts set into list of symbols"
  (let (result)
    (loop for op in *operators* do
	 (when (op-test op set)
	   (push op result))
	 finally (return result))))

	     