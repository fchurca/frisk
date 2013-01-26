(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp)
  (:export :territory
           :name
           :extra-armies
           :owner
           :armies
           :player
           :read-map
           :move-armies))

(in-package :ar.com.fchurca.frisk)

; (ql:quickload "cl-graph")

(defclass territory ()
  ((name
     :initarg :name
     :initform (error "Debe ingresar el nombre del territorio")
     :reader name)
   (extra-armies
     :initarg :extra-armies
     :initform (error "Debe ingresar la cantidad de ejércitos adicionales del territorio")
     :reader extra-armies)
   (owner
     :initform nil
     :accessor owner)
   (armies
     :initform 0
     :accessor armies)))

(defclass player ()
  ((name
     :initarg :name
     :initform (error "Debe ingresar el nombre del jugador")
     :reader name)))

(defgeneric read-map (file))

(defmethod read-map ((file stream))
  (let ((*read-eval* nil)
        (territories (make-hash-table))
        (filecontents (read file)))
    (dolist (territory (getf filecontents :territories))
      (setf (gethash (car territory) territories)
            (make-instance 'territory
                           :name (second territory)
                           :extra-armies (third territory))))
    territories))

(defmethod read-map ((path pathname))
  (with-open-file (file path) (read-map file)))

(defmethod read-map ((path string))
  (read-map
    (parse-namestring
      (if (probe-file path)
        path
        (format nil "~a.map" path)))))

(defun move-armies (origin destination amount)
  (unless (> amount 0)
    (error "No se pueden mover 0 o menos ejércitos"))
  (unless (> (armies origin) amount)
    (error "No hay suficientes ejércitos"))
  (decf (armies origin) amount)
  (incf (armies destination) amount))

