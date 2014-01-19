(in-package :ar.com.fchurca.frisk)

#|
(defparameter *fsm*
  (make-fsm
    (
     foo (
          baz bar
          quz foo)
     bar (
          qux ((fsm string)
               (print string)
               (switch fsm 'foo))
          quux ((fsm)
                t)))
    foo))
;*FSM*
(states *fsm*)
;(FOO BAR)
(state *fsm*)
;FOO
(available-messages *fsm*)
;(BAZ QUZ)
(send *fsm* 'quz)
;FOO
(send *fsm* 'baz)
;BAR
(send *fsm* 'quux)
;T
(send *fsm* 'qux "Hello, world!")
;"Hello, world!"
;FOO
|#

(defclass fsm ()
  ((states :initform nil :initarg :states)
   (state :initform nil :reader state :initarg :state)))

(defmacro make-fsm (states initial-state)
  `(make-instance
     'fsm :states
     (list
       ,@(loop for (name spec) on states by #'cddr appending
               `(',name
                 (list
                   ,@(loop for (message spec) on spec by #'cddr appending
                           `(',message
                             ,(if (listp spec)
                                (destructuring-bind
                                  ((&whole params fsm &rest rest) &body body) spec
                                  (declare (ignore rest))
                                  `(lambda ,params
                                     (declare (ignorable ,fsm))
                                     ,@body))
                                `',spec)))))))
     :state ',initial-state))

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
