
(defclass Snumber () ((value :accessor number-value :initarg :value)))
(defmethod to_s ((x Snumber)) (format nil "~A" (number-value x)))
(defmethod reduciblep ((x Snumber)) nil)
(defmethod myreduce ((x Snumber) env) (number-value x))


(defclass Sadd () ((left :accessor add-left :initarg :left)
				  (right :accessor add-right :initarg :right)))
(defmethod to_s ((x Sadd)) (format nil "~A + ~A" (to_s (add-left x)) (to_s (add-right x))))
(defmethod reduciblep ((x Sadd)) t)
(defmethod myreduce ((x Sadd) env) (cond ((reduciblep (add-left x)) (make-instance 'Sadd :left (myreduce (add-left x) env) :right (add-right x)))
								   ((reduciblep (add-right x)) (make-instance 'Sadd :left (add-left x) :right (myreduce (add-right x) env)))
								   (t (make-instance 'Snumber :value (+ (myreduce (add-left x) env) (myreduce (add-right x) env))))))


(defclass Smultiply () ((left :accessor multiply-left :initarg :left)
					 (right :accessor multiply-right :initarg :right)))
(defmethod to_s ((x Smultiply)) (format nil "~A * ~A" (to_s (multiply-left x)) (to_s (multiply-right x))))
(defmethod reduciblep ((x Smultiply)) t)
(defmethod myreduce ((x Smultiply) env) (cond ((reduciblep (multiply-left x)) (make-instance 'Smultiply :left (myreduce (multiply-left x) env) :right (multiply-right x)))
										((reduciblep (multiply-right x)) (make-instance 'Smultiply :left (multiply-left x) :right (myreduce (multiply-right x) env)))
										(t (make-instance 'Snumber :value (* (myreduce (multiply-left x) env) (myreduce (multiply-right x) env))))))


(defclass Sboolean () ((value :accessor boolean-value :initarg :value)))
(defmethod to_s ((x Sboolean)) (if (boolean-value x) "true" "false"))
(defmethod reduciblep ((x Sboolean)) nil)
(defmethod myreduce ((x Sboolean) env) (boolean-value x))


(defclass Slessthan () ((left :accessor lessthan-left :initarg :left)
						(right :accessor lessthan-right :initarg :right)))
(defmethod to_s ((x Slessthan)) (format nil "~A < ~A" (to_s (lessthan-left x)) (to_s (lessthan-right x))))
(defmethod reduciblep ((x Slessthan)) t)
(defmethod myreduce ((x Slessthan) env) (cond ((reduciblep (lessthan-left x)) (make-instance 'Slessthan :left (myreduce (lessthan-left x) env) :right (lessthan-right x)))
										  ((reduciblep (lessthan-right x)) (make-instance 'Slessthan :left (lessthan-left x) :right (myreduce (lessthan-right x) env)))
										  (t (make-instance 'Sboolean :value (< (myreduce (lessthan-left x) env) (myreduce (lessthan-right x) env))))))


(defclass Svariable () ((name :accessor variable-name :initarg :aname)))
(defmethod to_s ((x Svariable)) (format nil "~A" (variable-name x)))
(defmethod reduciblep ((x Svariable)) t)
(defmethod myreduce ((x Svariable) env) (cdr (assoc (variable-name x) env)))


(defclass Sdonothing () (()))
(defmethod to_s ((x Sdonothing)) "do-nothing")
(defmethod reduciblep ((x Sdonothing)) nil)
(defmethod myreduce ((x Sdonothing) env) x)


(defclass Machine () ((expression :accessor machine-expression :initarg :expression)
					  (environment :accessor machine-environment :initarg :environment)))
(defmethod run ((m Machine))
  (labels ((rec (step)
				(myinspect step)
				(when (reduciblep step)
				  (rec (myreduce step (machine-environment m)))
				  )))
	(format t "start ~%")
	(rec (machine-expression m))))


(defun myinspect (x) (format t "<< ~A >>~%" (to_s x)))
(defun inspect_reduce(a)
  (format t "~A~%" (myinspect a))
  (when (reduciblep a) (format t "~A~%" (myinspect (myreduce a nil)))))
(inspect_reduce (make-instance 'Sadd :left (make-instance 'Snumber :value 9) :right (make-instance 'Snumber :value 7)))
(inspect_reduce (make-instance 'Smultiply :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)))

(let ((m0 (make-instance 'Machine :expression (make-instance 'Sadd :left (make-instance 'Smultiply :left (make-instance 'Snumber :value 1) :right (make-instance 'Snumber :value 2)) :right (make-instance 'Smultiply :left (make-instance 'Snumber :value 3) :right (make-instance 'Snumber :value 4))) :environment nil))
	  (m1 (make-instance 'Machine :expression (make-instance 'Slessthan :left (make-instance 'Sadd :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)) :right (make-instance 'Snumber :value 6)) :environment nil))
	  (m2 (make-instance 'Machine :expression (make-instance 'Slessthan :left (make-instance 'Sadd :left (make-instance 'Snumber :value 2) :right (make-instance 'Snumber :value 3)) :right (make-instance 'Snumber :value 5)) :environment nil))
	  (m3 (make-instance 'Machine :expression (make-instance 'Svariable :aname 'x) :environment (list (cons 'x (make-instance 'Snumber :value 8)))))
	  (m4 (make-instance 'Machine :expression (make-instance 'Sadd :left (make-instance 'Svariable :aname 'x) :right (make-instance 'Svariable :aname 'y)) :environment (list (cons 'x (make-instance 'Snumber :value 8)) (cons 'y (make-instance 'Snumber :value 9)))))
	  )
  (run m0)
  (run m1)
  (run m2)
  (run m3)
  (run m4)
  )

