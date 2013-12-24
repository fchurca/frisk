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
  ((game-map nil r)
   (players nil r)
   (head-player nil)
   (turn-player nil)))

(defgeneric read-map (file)
  (:method ((list list))
   (let* ((territories (make-hash-table :test 'equalp))
          (frontiers (make-container 'graph-container)))
     (dolist (territory (getf list :territories))
       (destructuring-bind (key name extra-armies terr-frontiers) territory
         (add-vertex frontiers key)
         (setf (gethash key territories)
               (make-instance 'territory
                              :name name
                              :extra-armies extra-armies))
         (dolist (frontierkey terr-frontiers)
           (add-edge-between-vertexes frontiers key frontierkey))))
     (make-instance 'game-map :territories territories :frontiers frontiers)))

  (:method ((file stream))
   (let* ((*read-eval* nil))
     (read-map (read file))))

  (:method ((path pathname))
   (with-open-file (file path) (read-map file)))

  (:method ((path string))
   (read-map
     (parse-namestring
       (if (probe-file path)
         path
         (format nil "~a.map" path))))))

(defgeneric read-game (file)
  (:method ((list list))
   (let ((game (make-instance 'game
                              :game-map (read-map (getf list :game-map))
                              :players (getf list :players))))
     (loop for (name armies-dist) on (getf list :armies) by #'cddr do
           (loop for (territory-key armies) on armies-dist by #'cddr
                 for territory = (territory game territory-key) do
                 (setf (owner territory) (player game name))
                 (setf (armies territory) armies)))
     ; TODO: signal error on uninitialized territories
     game))

  (:method ((file stream))
   (let ((*read-eval* nil))
     (read-game (read file))))

  (:method ((path pathname))
   (with-open-file (file path) (read-game file)))

  (:method ((path string))
   (read-game
     (parse-namestring
       (if (probe-file path)
         path
         (format nil "~a.save" path))))))

(defmethod initialize-instance ((game game) &key game-map players)
  (with-slots ((the-game-map game-map)
               (the-players players)
               (the-head-player head-player)
               (the-turn-player turn-player)) game
    (setf the-game-map
          (typecase game-map
            (game-map game-map)
            (t (read-map game-map))))
    (setf the-players (make-hash-table :test 'equalp))
    (dolist (name players) (add-player game name))  
    (setf the-head-player (copy-list players))
    (setf the-turn-player (copy-list players))))

(defgeneric territory (container key)
  (:method ((container game-map) key)
   (gethash key (territories container)))
  (:method ((container game) key)
   (territory (game-map container) key)))

(defgeneric head-player (game)
  (:method ((game game)) (car (slot-value game 'head-player))))

(defgeneric turn-player (game)
  (:method ((game game)) (car (slot-value game 'turn-player))))

(defgeneric pass-head (game)
  (:method ((game game))
   (with-slots ((the-head-player head-player)) game
     (let ((head (pop the-head-player)))
       (setf the-head-player (nconc the-head-player (list head)))))
   (head-player game)))

(defgeneric pass-turn (game)
  (:method ((game game))
   (with-slots ((the-turn-player turn-player)
                (the-head-player head-player)) game
     (pop the-turn-player)
     (unless the-turn-player
       (setf the-turn-player (copy-list the-head-player))))
   (turn-player game)))

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
     (loop
       with territories = (shuffle (territory-keys game))
       for player in (let ((players (player-keys game)))
                       (nconc players players))
       for territory in territories do
       (setf (owner (territory game territory)) (player game player))
       (setf (armies (territory game territory)) 1)))))

(defgeneric move-armies (game from to amount)
  (:method ((game game) origin-key destination-key amount)
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

(defgeneric place-armies (game where amount)
  (:method ((game game) territory-key (amount integer))
   (let ((territory (territory game territory-key)))
     (unless (> amount 0)
       (error "No se pueden poner 0 o menos ejércitos"))
     (incf (armies territory) amount))))

(defgeneric attack (game from to)
  (:method ((game game) origin-key destination-key)
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

(defmethod print-object ((game game) stream)
  (format stream "~&Jugadores:~t~{~a~^, ~}~&Cabecera:~t~a~&Turno:~t~a"
          (loop for v being the hash-values in (players game)
                collecting (name v))
          (head-player game)
          (turn-player game))
  (format stream "~&Territorios:")
  (loop for v being the hash-values in (territories game) do
        (format stream "~&~a~&~tDueño:~t~a~&~tEjércitos:~t~a"
                (name v)
                (if (owner v) (name (owner v)) "Ninguno")
                (if (armies v) (armies v) "Ninguno"))))

