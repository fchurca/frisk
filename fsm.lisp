#|
(defparameter *fsm*
  (make-instance 'fsm
                 :states `(
                           :foo (
                                 :baz :bar
                                 :quz :foo)
                           :bar (
                                 :qux ,(lambda (fsm string)
                                         (print string)
                                         (switch fsm :foo))
                                 :quux ,(lambda (fsm)
                                          t)))
                 :state :foo))
;*fsm*
(states *fsm*)
;(:FOO :BAR)
(state *fsm*)
;:FOO
(available-messages *fsm*)
;(:BAZ :QUZ)
(send *fsm* :quz)
;T
(send *fsm* :baz)
;:BAR
(send *fsm* :quux)
;T
(send *fsm* :qux "Hello, world!")
;"Hello, world!"
;:FOO
|#

(defclass fsm ()
  ((states :initform nil :initarg :states)
   (state :initform nil :reader state :initarg :state)))

(defgeneric switch (fsm state)
  (:method ((fsm fsm) state)
   (if (find state (states fsm))
     (setf (slot-value fsm 'state) state)
     (error (format nil "Machine ~a doesn't have state ~a" fsm state)))))

(defgeneric states (fsm)
  (:method ((fsm fsm))
   (loop for (key value) on (slot-value fsm 'states) by #'cddr
         collecting key)))

(defgeneric available-messages (fsm)
  (:method ((fsm fsm))
   (loop for (key value) on (getf (slot-value fsm 'states) (state fsm)) by #'cddr
         collecting key)))

(defgeneric send (fsm message &rest rest)
  (:method ((fsm fsm) message &rest rest)
   (let ((callback (getf (getf (slot-value fsm 'states) (state fsm)) message)))
     (typecase callback
       (function
         (apply callback fsm rest))
       (null
         (error (format nil "Machine ~a can't receive message ~a" fsm message)))
       (t (switch fsm callback))))))
