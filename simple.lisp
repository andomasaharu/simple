
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


(defun make-donothing () (make-instance 'Sdonothing))
(defclass Sdonothing () (()))
(defmethod to_s ((x Sdonothing)) "do-nothing")
(defmethod reduciblep ((x Sdonothing)) nil)
(defmethod myreduce ((x Sdonothing) env) x)
(defmethod donothingp ((x Sdonothing)) t)
(defmethod donothingp (x) nil)


(defun make-assign (n e) (make-instance 'Sassign :name n :expression e))
(defclass Sassign () ((name :accessor assign-name :initarg :name)
					  (expression :accessor assign-expression :initarg :expression)))
(defmethod to_s ((x Sassign)) (format nil "~A = ~A" (assign-name x) (to_s (assign-expression x))))
(defmethod reduciblep ((x Sassign)) t)
(defmethod myreduce ((x Sassign) env) (if (reduciblep (assign-expression x))
										(make-assign (assign-name x) (myreduce (assign-expression x) env))
										(cons (make-donothing) (cons (cons (assign-name x) (assign-expression x)) env))))


(defun make-if (cnd cns alt) (make-instance 'Sif :condition cnd :consequence cns :alternative alt))
(defclass Sif () ((condition :accessor if-condition :initarg :condition)
				  (consequence :accessor if-consequence :initarg :consequence)
				  (alternative :accessor if-alternative :initarg :alternative)))
(defmethod to_s ((x Sif)) (format nil "if ~A then ~A else ~A" (to_s (if-condition x)) (to_s (if-consequence x)) (to_s (if-alternative x))))
(defmethod reduciblep ((x Sif)) t)
(defmethod myreduce ((x Sif) env) (if (reduciblep (if-condition x))
									(make-if (myreduce (if-condition x) env) (if-consequence x) (if-alternative x))
									(if (if-condition x)
									  (myreduce (if-consequence x) env)
									  (myreduce (if-alternative x) env))))


(defun make-machine (expr env) (make-instance 'Machine :expression expr :environment env))
(defclass Machine () ((expression :accessor machine-expression :initarg :expression)
					  (environment :accessor machine-environment :initarg :environment)))
(defmethod run ((m Machine))
  (labels ((rec (step)
				(myinspect step)
				(when (reduciblep step)
				  (let ((r (myreduce step (machine-environment m))))
					  (rec r))
				  )))
	(format t "start ~%")
	(rec (machine-expression m))))


(defmethod to_s (x) (format nil "~A" x))
(defmethod reduciblep (x) nil)
(defmethod myreduce (x env) x)

(defun myinspect (x) (format t "<< ~A >>~%" (to_s x)))
(defun inspect_reduce(a env)
  (format t "~A~%" (myinspect a))
  (when (reduciblep a) (format t "~A~%" (myinspect (myreduce a env)))))
(inspect_reduce (make-if (make-lt (make-num 1) (make-num 2)) (make-assign 'y (make-num 1)) (make-assign 'y (make-num 2))) nil)
(inspect_reduce (make-add (make-num 9) (make-num 7)) nil)
(inspect_reduce (make-mul (make-num 2) (make-num 3)) nil)
(myinspect (make-assign 'x (make-add (make-var 'x) (make-num 1))))
(let ((env '((x . 2))))
  (myinspect (myreduce (myreduce (make-assign 'x (make-add (make-var 'x) (make-num 1))) env) env)))

(let ((tests (list
			   (make-machine (make-add (make-mul (make-num 1) (make-num 2)) (make-mul (make-num 3) (make-num 4))) nil)
			   (make-machine (make-lt (make-add (make-num 2) (make-num 3)) (make-num 6)) nil)
			   (make-machine (make-lt (make-add (make-num 2) (make-num 3)) (make-num 5)) nil)
			   (make-machine (make-var 'x) (list (cons 'x (make-num 8))))
			   (make-machine (make-add (make-var 'x) (make-var 'y)) (list (cons 'x (make-num 8)) (cons 'y (make-num 9))))
			   (make-machine (make-assign 'x (make-var 'x) ) '((x . 1)))
			   (make-machine (make-assign 'x (make-add (make-var 'x) (make-num 2))) '((x . 1)))
			   (make-machine (make-if (make-lt (make-num 1) (make-num 2)) (make-assign 'y (make-num 1)) (make-assign 'y (make-num 2))) nil)
			   (make-machine (make-if (make-lt (make-num 3) (make-num 2)) (make-assign 'y (make-num 1)) (make-assign 'y (make-num 2))) nil)
			   )))
  (mapcar #'run tests))

