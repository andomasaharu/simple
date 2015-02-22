
(defclass Snumber () ((value :accessor number-value :initarg :value)))
(defmethod to_s ((x Snumber)) (format nil "~A" (number-value x)))
(defmethod reduciblep ((x Snumber)) nil)
(defmethod myreduce ((x Snumber)) (number-value x))


(defclass Sadd () ((left :accessor add-left :initarg :left)
				  (right :accessor add-right :initarg :right)))
(defmethod to_s ((x Sadd)) (format nil "~A + ~A" (to_s (add-left x)) (to_s (add-right x))))
(defmethod reduciblep ((x Sadd)) t)
(defmethod myreduce ((x Sadd)) (cond ((reduciblep (add-left x)) (make-instance 'Sadd :left (myreduce (add-left x)) :right (add-right x)))
								   ((reduciblep (add-right x)) (make-instance 'Sadd :left (add-left x) :right (myreduce (add-right x))))
								   (t (make-instance 'Snumber :value (+ (myreduce (add-left x)) (myreduce (add-right x)))))))


(defclass Smultiply () ((left :accessor multiply-left :initarg :left)
					 (right :accessor multiply-right :initarg :right)))
(defmethod to_s ((x Smultiply)) (format nil "~A * ~A" (to_s (multiply-left x)) (to_s (multiply-right x))))
(defmethod reduciblep ((x Smultiply)) t)
(defmethod myreduce ((x Smultiply)) (cond ((reduciblep (multiply-left x)) (make-instance 'Smultiply :left (myreduce (multiply-left x)) :right (multiply-right x)))
										((reduciblep (multiply-right x)) (make-instance 'Smultiply :left (multiply-left x) :right (myreduce (multiply-right x))))
										(t (make-instance 'Snumber :value (* (myreduce (multiply-left x)) (myreduce (multiply-right x)))))))


(defclass Sboolean () ((value :accessor boolean-value :initarg :value)))
(defmethod to_s ((x Sboolean)) (if (boolean-value x) "true" "false"))
(defmethod reduciblep ((x Sboolean)) nil)
(defmethod myreduce ((x Sboolean)) (boolean-value x))


(defclass Slessthan () ((left :accessor lessthan-left :initarg :left)
						(right :accessor lessthan-right :initarg :right)))
(defmethod to_s ((x Slessthan)) (format nil "~A < ~A" (to_s (lessthan-left x)) (to_s (lessthan-right x))))
(defmethod reduciblep ((x Slessthan)) t)
(defmethod myreduce ((x Slessthan)) (cond ((reduciblep (lessthan-left x)) (make-instance 'Slessthan :left (myreduce (lessthan-left x)) :right (lessthan-right x)))
										  ((reduciblep (lessthan-right x)) (make-instance 'Slessthan :left (lessthan-left x) :right (myreduce (lessthan-right x))))
										  (t (make-instance 'Sboolean :value (< (myreduce (lessthan-left x)) (myreduce (lessthan-right x)))))))


(defclass Machine () ((expression :accessor machine-expression :initarg :expression)))
(defmethod run ((m Machine))
  (labels ((rec (step)
				(myinspect step)
				(when (reduciblep step)
				  (rec (myreduce step))
				  )))
	(format t "start ~%")
	(rec (machine-expression m))))


(defun myinspect (x) (format t "<< ~A >>~%" (to_s x)))
(defun inspect_reduce(a)
  (format t "~A~%" (myinspect a))
  (when (reduciblep a) (format t "~A~%" (myinspect (myreduce a)))))
(inspect_reduce (make-instance 'Sadd :left (make-instance 'Snumber :value 9) :right (make-instance 'Snumber :value 7)))
(inspect_reduce (make-instance 'Smultiply :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)))

(let ((m0 (make-instance 'Machine :expression (make-instance 'Sadd :left (make-instance 'Smultiply :left (make-instance 'Snumber :value 1) :right (make-instance 'Snumber :value 2)) :right (make-instance 'Smultiply :left (make-instance 'Snumber :value 3) :right (make-instance 'Snumber :value 4)))))
	  (m1 (make-instance 'Machine :expression (make-instance 'Slessthan :left (make-instance 'Sadd :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)) :right (make-instance 'Snumber :value 6))))
	  (m2 (make-instance 'Machine :expression (make-instance 'Slessthan :left (make-instance 'Sadd :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)) :right (make-instance 'Snumber :value 5))))
	  )
  (run m0)
  (run m1)
  (run m2))

