(defpackage :ar.com.fchurca.fsm
  (:use :common-lisp
        :metabang-bind
        :iterate)
  (:import-from :metatilities :defclass*)
  (:import-from :alexandria :doplist)
  (:export
    :fsm
    :make-fsm
    :switch
    :states
    :state
    :available-messages
    :send))

(in-package :ar.com.fchurca.fsm)

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
               (_switch 'foo))
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

(defmacro-clause (FOR spec ON-PLIST plist)
  "The (key value)s of a property list"
  `(for (,(car spec) ,(cadr spec)) on ,plist by #'cddr))

(defclass* fsm ()
  ((states nil i)
   (state  nil ir)))

(defmacro make-fsm (states initial-state)
  (bind ((fsm-name (gensym))
         ((:flet message-spec->plist (spec))
          (if (listp spec)
            (destructuring-bind ((&whole params fsm &rest rest) &body body) spec
              (declare (ignore rest))
              `(lambda ,params
                 (declare (ignorable ,fsm))
                 ,@body))
            `',spec))
         ((:flet state-spec->plist (spec))
          `(list ,@(iter (for (message m-spec) on-plist spec)
                         (appending `(',message ,(message-spec->plist m-spec)))))))
    `(bind
       ((,fsm-name nil) 
        ((:flet _switch (state)) (switch ,fsm-name state)))
       (setf
         ,fsm-name
         (make-instance
           'fsm :states
           (list ,@(iter (for (name spec) on-plist states)
                         (appending `(',name ,(state-spec->plist spec)))))
           :state ',initial-state)))))

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
