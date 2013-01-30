(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp)
  (:export :territory
           :name
           :extra-armies
           :owner
           :armies
           :player
           :read-map
           :move-armies
           :place-armies
           :attack))

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
      (destructuring-bind (key name extra-armies) territory
        (setf (gethash key territories)
              (make-instance 'territory
                             :name name
                             :extra-armies extra-armies))))
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

(defun place-armies (territory amount)
  (unless (> amount 0)
    (error "No se pueden poner 0 o menos ejércitos"))
  (incf (armies territory) amount))

(defun attack (origin destination)
  (symbol-macrolet ((attackers (armies origin))
                    (defenders (armies destination)))
    (unless (> attackers 2)
      (error "No se puede atacar teniendo 2 o menos ejércitos"))
    (when (equal (owner destination) (owner origin))
      (error "Un jugador no se puede atacar a si mismo"))
    (let ((decrement (min defenders (- attackers 2))))
      (decf attackers decrement)
      (decf defenders decrement)
      (when (= defenders 0)
        (move-armies origin destination 1)
        (setf (owner destination) (owner origin))))
    (owner destination)))

