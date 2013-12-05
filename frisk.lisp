(in-package :ar.com.fchurca.frisk)

(defclass* territory ()
  ((name (error "Debe ingresar el nombre del territorio") ir)
   (extra-armies
     (error "Debe ingresar la cantidad de ejércitos adicionales del territorio")
     ir)
   (owner nil a)
   (armies nil a)))

(defclass* game-map ()
  ((territories (error "Debe especificar los territorios") ir)
   (frontiers (error "Debe especificar las fronteras") ir)))

(defclass* player ()
  ((name (error "Debe ingresar el nombre del jugador") ir)))

(defclass* game ()
  ((game-map (error "Debe especificar el mapa") ir)
   (players (make-hash-table :test 'equalp) ir)))

(defgeneric read-map (file &key)
  (:method ((file stream) &key)
   (let ((*read-eval* nil)
         (territories (make-hash-table :test 'equalp))
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
     (make-instance 'game-map :territories territories :frontiers frontiers)))

  (:method ((path pathname) &key)
   (with-open-file (file path) (read-map file)))

  (:method ((path string) &key)
   (read-map
     (parse-namestring
       (if (probe-file path)
         path
         (format nil "~a.map" path))))))

(defgeneric territory (container key)
  (:method ((container game-map) key)
   (gethash key (territories container)))
  (:method ((container game) key)
   (territory (game-map container) key)))

(defmethod territories ((game game))
  (territories (game-map game)))

(defmethod frontiers ((game game))
  (frontiers (game-map game)))

(defgeneric player (game player-key)
  (:method ((game game) player-key)
   (gethash player-key (players game))))

(defgeneric territory-keys (game)
  (:method ((game game))
   (hash-table-keys (territories game))))

(defgeneric player-keys (game)
  (:method ((game game))
   (hash-table-keys (players game))))

(defgeneric territories-connected-p (game origin-key destination-key)
  (:method ((game game) origin-key destination-key)
   (let ((graph (frontiers game)))
     (vertices-share-edge-p
       (find-vertex graph origin-key)
       (find-vertex graph destination-key)))))

(defgeneric add-player (game name)
  (:method ((game game) (name string))
   (setf (gethash name (players game)) (make-instance 'player :name name))))

(defgeneric shuffle-territories (game)
  (:method ((game game))
   (let ((territory-count (size (territories game)))
         (player-count (size (players game))))
     (when (= territory-count 0)
       (error "El juego debe tener territorios"))
     (when (= player-count 0)
       (error "El juego debe tener jugadores"))
     (unless (= (rem territory-count player-count) 0)
       (error "Los territorios deben ser divisibles entre los jugadores"))
     (flet
       ((initialize-territory (territory player)
          (setf (owner (territory game territory)) (player game player))
          (setf (armies (territory game territory)) 1)))
       (let* ((territories (shuffle (territory-keys game)))
              (players (shuffle (player-keys game))))
         (loop
           for player = (first (nconc players players))
                      then (progn (setf players (rest players)) (first players))
           for territory in territories
           while territory do
           (initialize-territory territory player)))))))

(defgeneric move-armies (game from to amount &key)
  (:method ((game game) origin-key destination-key amount &key)
   (let ((origin (territory game origin-key))
         (destination (territory game destination-key)))
     (unless (territories-connected-p game origin-key destination-key)
       (error "Los territorios deben estar conectados"))
     (unless (eq (owner origin) (owner destination))
       (error "Los territorios tienen que tener el mismo dueño"))
     (unless (> amount 0)
       (error "No se pueden mover 0 o menos ejércitos"))
     (unless (> (armies origin) amount)
       (error "No hay suficientes ejércitos"))
     (decf (armies origin) amount)
     (incf (armies destination) amount))))

(defgeneric place-armies (game where amount &key)
  (:method ((game game) territory-key (amount integer) &key)
   (let ((territory (territory game territory-key)))
     (unless (> amount 0)
       (error "No se pueden poner 0 o menos ejércitos"))
     (incf (armies territory) amount))))

(defgeneric attack (game from to &key)
  (:method ((game game) origin-key destination-key &key)
   (symbol-macrolet
     ((origin (territory game origin-key))
      (destination (territory game destination-key))
      (attackers (armies origin))
      (defenders (armies destination)))
     (unless (territories-connected-p game origin-key destination-key)
       (error "Los territorios deben estar conectados"))
     (unless (> attackers 2)
       (error "No se puede atacar teniendo 2 o menos ejércitos"))
     (when (equal (owner destination) (owner origin))
       (error "Un jugador no se puede atacar a sí mismo"))
     (let ((decrement (1+ (min defenders (- attackers 2)))))
       (decf attackers (random decrement))
       (decf defenders (random decrement))
       (when (= defenders 0)
         (setf (owner destination) (owner origin))
         (move-armies game origin-key destination-key 1)))
     (eq (owner origin) (owner destination)))))

(defgeneric print-game (game)
  (:method ((game game))
   (format t "~&Jugadores:")
   (loop for v being the hash-values in (players game) do
         (format t "~&~t~a" (name v)))
   (format t "~&Territorios:")
   (loop for v being the hash-values in (territories game) do
         (format t "~&~a~&~tDueño:~t~a~&~tEjércitos:~a"
                 (name v)
                 (if (owner v) (name (owner v)) "Ninguno")
                 (armies v)))))

