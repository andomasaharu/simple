
(defun make-num (a) (make-instance 'Snumber :value a))
(defclass Snumber () ((value :accessor number-value :initarg :value)))
(defmethod to_s ((x Snumber)) (format nil "~A" (number-value x)))
(defmethod reduciblep ((x Snumber)) nil)
(defmethod myreduce ((x Snumber) env) (number-value x))


(defun make-add (l r) (make-instance 'Sadd :left l :right r))
(defclass Sadd () ((left :accessor add-left :initarg :left)
				   (right :accessor add-right :initarg :right)))
(defmethod to_s ((x Sadd)) (format nil "~A + ~A" (to_s (add-left x)) (to_s (add-right x))))
(defmethod reduciblep ((x Sadd)) t)
(defmethod myreduce ((x Sadd) env) (cond ((reduciblep (add-left x)) (make-add (myreduce (add-left x) env) (add-right x)))
										 ((reduciblep (add-right x)) (make-add (add-left x) (myreduce (add-right x) env)))
										 (t (make-num (+ (myreduce (add-left x) env) (myreduce (add-right x) env))))))


(defun make-mul (l r) (make-instance 'Smultiply :left l :right r))
(defclass Smultiply () ((left :accessor multiply-left :initarg :left)
						(right :accessor multiply-right :initarg :right)))
(defmethod to_s ((x Smultiply)) (format nil "~A * ~A" (to_s (multiply-left x)) (to_s (multiply-right x))))
(defmethod reduciblep ((x Smultiply)) t)
(defmethod myreduce ((x Smultiply) env) (cond ((reduciblep (multiply-left x)) (make-mul (myreduce (multiply-left x) env) (multiply-right x)))
										((reduciblep (multiply-right x)) (make-mul (multiply-left x) (myreduce (multiply-right x) env)))
										(t (make-num (* (myreduce (multiply-left x) env) (myreduce (multiply-right x) env))))))


(defun make-bool (a) (make-instance 'Sboolean :value a))
(defclass Sboolean () ((value :accessor boolean-value :initarg :value)))
(defmethod to_s ((x Sboolean)) (if (boolean-value x) "true" "false"))
(defmethod reduciblep ((x Sboolean)) nil)
(defmethod myreduce ((x Sboolean) env) (boolean-value x))


(defun make-lt (l r) (make-instance 'Slessthan :left l :right r))
(defclass Slessthan () ((left :accessor lessthan-left :initarg :left)
						(right :accessor lessthan-right :initarg :right)))
(defmethod to_s ((x Slessthan)) (format nil "~A < ~A" (to_s (lessthan-left x)) (to_s (lessthan-right x))))
(defmethod reduciblep ((x Slessthan)) t)
(defmethod myreduce ((x Slessthan) env) (cond ((reduciblep (lessthan-left x)) (make-lt (myreduce (lessthan-left x) env) (lessthan-right x)))
											  ((reduciblep (lessthan-right x)) (make-lt (lessthan-left x) (myreduce (lessthan-right x) env)))
											  (t (make-bool (< (myreduce (lessthan-left x) env) (myreduce (lessthan-right x) env))))))


(defun make-var (a) (make-instance 'Svariable :aname a))
(defclass Svariable () ((name :accessor variable-name :initarg :aname)))
(defmethod to_s ((x Svariable)) (format nil "~A" (variable-name x)))
(defmethod reduciblep ((x Svariable)) t)
(defmethod myreduce ((x Svariable) env) (cdr (assoc (variable-name x) env)))


(defclass Sdonothing () (()))
(defmethod to_s ((x Sdonothing)) "do-nothing")
(defmethod reduciblep ((x Sdonothing)) nil)
(defmethod myreduce ((x Sdonothing) env) x)


(defun make-machine (expr env) (make-instance 'Machine :expression expr :environment env))
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
(inspect_reduce (make-add (make-num 9) (make-num 7)))
(inspect_reduce (make-mul (make-num 2) (make-num 3)))

(let ((tests (list
			   (make-machine (make-add (make-mul (make-num 1) (make-num 2)) (make-mul (make-num 3) (make-num 4))) nil)
			   (make-machine (make-lt (make-add (make-num 2) (make-num 3)) (make-num 6)) nil)
			   (make-machine (make-lt (make-add (make-num 2) (make-num 3)) (make-num 5)) nil)
			   (make-machine (make-var 'x) (list (cons 'x (make-num 8))))
			   (make-machine (make-add (make-var 'x) (make-var 'y)) (list (cons 'x (make-num 8)) (cons 'y (make-num 9))))
			   )))
  (mapcar #'run tests))

