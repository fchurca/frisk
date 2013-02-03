(in-package :ar.com.fchurca.frisk)

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

(defclass game ()
  ((turn-player
     :accessor turn-player)
   (territories
     :initarg :territories
     :accessor territories
     :initform (error "Debe especificar los territorios"))))

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
    (make-instance 'game :territories territories)))

(defmethod read-map ((path pathname))
  (with-open-file (file path) (read-map file)))

(defmethod read-map ((path string))
  (read-map
    (parse-namestring
      (if (probe-file path)
        path
        (format nil "~a.map" path)))))

(defun territory (game territory-key)
  (gethash territory-key (territories game)))
 
(defun move-armies (game origin-key destination-key amount)
  (let ((origin (territory game origin-key))
        (destination (territory game destination-key)))
    (unless (eq (owner origin) (owner destination))
      (error "Los territorios tienen que tener el mismo dueno"))
    (unless (eq (owner origin) (turn-player game))
      (error "El jugador de turno no controla esos territorios"))
    (unless (> amount 0)
      (error "No se pueden mover 0 o menos ejércitos"))
    (unless (> (armies origin) amount)
      (error "No hay suficientes ejércitos")) 
    (decf (armies origin) amount) 
    (incf (armies destination) amount)))

(defun place-armies (game territory-key amount)
  (let ((territory (territory game territory-key)))
    (unless (> amount 0)
      (error "No se pueden poner 0 o menos ejércitos"))
    (unless (eq (owner territory) (turn-player game))
      (error "El jugador de turno no controla ese territorio")) 
    (incf (armies territory) amount)))

(defun attack (game origin-key destination-key)
  (symbol-macrolet
    ((origin (territory game origin-key))
     (destination (territory game destination-key))
     (attackers (armies origin))
     (defenders (armies destination)))
    (unless (> attackers 2)
      (error "No se puede atacar teniendo 2 o menos ejércitos"))
    (unless (eq (owner origin) (turn-player game))
      (error "El jugador de turno no controla esos territorios"))
    (when (equal (owner destination) (owner origin))
      (error "Un jugador no se puede atacar a si mismo"))
    (let ((decrement (min defenders (- attackers 2))))
      (decf attackers decrement)
      (decf defenders decrement)
      (when (= defenders 0)
        (setf (owner destination) (owner origin))
        (move-armies game origin-key destination-key 1)))
    (owner destination)))

