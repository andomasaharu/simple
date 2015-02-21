(defclass Snumber () ((value :accessor number-value :initarg :value)))

(defmethod to_s ((x Snumber)) (format nil "~A" (number-value x)))

(defclass Sadd () ((left :accessor add-left :initarg :left)
				  (right :accessor add-right :initarg :right)))
(defmethod to_s ((x Sadd)) (format nil "~A + ~A" (to_s (add-left x)) (to_s (add-right x))))

(defclass Smultiply () ((left :accessor multiply-left :initarg :left)
					 (right :accessor multiply-right :initarg :right)))
(defmethod to_s ((x Smultiply)) (format nil "~A * ~A" (to_s (multiply-left x)) (to_s (multiply-right x))))
(defun myinspect (x) (format t "<<~A>>~%" (to_s x)))

(defmethod reduciblep ((x Snumber)) nil)
(defmethod reduciblep ((x Sadd)) t)
(defmethod reduciblep ((x Smultiply)) t)

(format t "~A~%" (myinspect (make-instance 'Sadd :left (make-instance 'Smultiply :left (make-instance 'Snumber :value 1) :right (make-instance 'Snumber :value 2)) :right (make-instance 'Smultiply :left (make-instance 'Snumber :value 3) :right (make-instance 'Snumber :value 4)))))

