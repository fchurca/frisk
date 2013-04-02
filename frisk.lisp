(in-package :ar.com.fchurca.frisk)

(defclass* territory ()
  ((name (error "Debe ingresar el nombre del territorio") ir)
   (extra-armies
     (error "Debe ingresar la cantidad de ejércitos adicionales del territorio")
     ir)
   (owner nil a)
   (armies 0 a)))

(defclass* player ()
  ((name (error "Debe ingresar el nombre del jugador") ir)))

(defclass* game ()
  ((turn-player nil a)
   (territories (error "Debe especificar los territorios") ir)
   (frontiers (error "Debe especificar las fronteras") ir)))

(defgeneric read-map (file &key)
  (:method ((file stream) &key)
   (let ((*read-eval* nil)
         (territories (make-hash-table))
         (frontiers (make-container 'graph-container))
         (filecontents (read file)))
     (dolist (territory (getf filecontents :territories))
       (destructuring-bind (key name extra-armies terr-frontiers) territory
         (add-vertex frontiers key)
         (setf (gethash key territories)
               (make-instance 'territory
                              :name name
                              :extra-armies extra-armies))
         (dolist (frontierkey terr-frontiers)
           (add-edge-between-vertexes frontiers key frontierkey))))
     (make-instance 'game :territories territories :frontiers frontiers))) 

  (:method ((path pathname) &key)
   (with-open-file (file path) (read-map file))) 

  (:method ((path string) &key)
   (read-map
     (parse-namestring
       (if (probe-file path)
         path
         (format nil "~a.map" path)))))) 

(defun territory (game territory-key)
  (gethash territory-key (territories game)))

(defun territory-keys (game)
  (loop for k being the hash-keys in (territories game) collecting k))

(defun territories-connected-p (game origin-key destination-key)
  (let ((graph (frontiers game)))
    (vertices-share-edge-p
      (find-vertex graph origin-key)
      (find-vertex graph destination-key))))

(defgeneric move-armies (game from to amount &key))

(defmethod move-armies (game origin-key destination-key amount &key)
  (let ((origin (territory game origin-key))
        (destination (territory game destination-key)))
    (unless (territories-connected-p game origin-key destination-key)
      (error "Los territorios deben estar conectados"))
    (unless (eq (owner origin) (owner destination))
      (error "Los territorios tienen que tener el mismo dueño"))
    (unless (eq (owner origin) (turn-player game))
      (error "El jugador de turno no controla esos territorios"))
    (unless (> amount 0)
      (error "No se pueden mover 0 o menos ejércitos"))
    (unless (> (armies origin) amount)
      (error "No hay suficientes ejércitos")) 
    (decf (armies origin) amount) 
    (incf (armies destination) amount)))

(defgeneric place-armies (game where amount &key))

(defmethod place-armies (game territory-key amount &key)
  (let ((territory (territory game territory-key)))
    (unless (> amount 0)
      (error "No se pueden poner 0 o menos ejércitos"))
    (unless (eq (owner territory) (turn-player game))
      (error "El jugador de turno no controla ese territorio")) 
    (incf (armies territory) amount)))

(defgeneric attack (game from to &key))

(defmethod attack (game origin-key destination-key &key)
  (symbol-macrolet
    ((origin (territory game origin-key))
     (destination (territory game destination-key))
     (attackers (armies origin))
     (defenders (armies destination)))
    (unless (territories-connected-p game origin-key destination-key)
      (error "Los territorios deben estar conectados"))
    (unless (> attackers 2)
      (error "No se puede atacar teniendo 2 o menos ejércitos"))
    (unless (eq (owner origin) (turn-player game))
      (error "El jugador de turno no controla ese territorio"))
    (when (equal (owner destination) (owner origin))
      (error "Un jugador no se puede atacar a sí mismo"))
    (let ((decrement (1+ (min defenders (- attackers 2)))))
      (decf attackers (random decrement))
      (decf defenders (random decrement))
      (when (= defenders 0)
        (setf (owner destination) (owner origin))
        (move-armies game origin-key destination-key 1)))
    (eq (owner origin) (owner destination))))

