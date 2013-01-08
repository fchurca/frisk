; (ql:quickload "cl-graph")

(defclass territory ()
  ((name
     :initarg :name
     :initform (error "Debe ingresar el nombre del país")
     :reader name)
   (extra-armies
     :initarg :extra-armies
     :initform (error "Debe ingresar la cantidad de ejércitos adicionales del país")
     :reader extra-armies)
   (frontiers
     :initform (make-hash-table)
     :reader frontiers)
   (owner
     :initform nil
     :accessor owner)
   (armies
     :initform 0
     :accessor armies)))

(defclass jugador ()
  ((name
     :initarg :name
     :initform (error "Debe ingresar el nombre del jugador")
     :reader name)
   (territories
     :initform (make-hash-table)
     :reader territories)))

(defun read-map (file)
  (let ((territories (make-hash-table))
        (filecontents (read file)))
    (dolist (territory (getf filecontents 'territories))
      (setf (gethash (car territory) territories)
            (make-instance 'territory :name (second territory) :extra-armies (third territory))))
    territories))

