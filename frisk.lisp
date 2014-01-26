(in-package :ar.com.fchurca.frisk)

(defclass* territory ()
  ((territory-name (error "Debe ingresar el nombre del territorio") ir)
   (extra-armies
     (error "Debe ingresar los ejércitos adicionales del territorio")
     ir)
   (%owner nil a :reader owner)
   (%armies nil a :reader armies)
   (%movable-armies nil a :reader movable-armies)))

(defclass* game-map ()
  ((territories (error "Debe especificar los territorios") ir)
   (frontiers (error "Debe especificar las fronteras") ir)
   (initial-armies (error "Debe especificar los ejércitos iniciales") ir)))

(defclass* player ()
  ((player-name (error "Debe ingresar el nombre del jugador") ir)
   (game (error "Debe especificar el juego vinculado al jugador") ir)))

(defclass* game ()
  ((game-map nil r)
   (game-fsm nil r)
   (%players (make-hash-table :test 'equalp) r)
   (%players-round nil a)
   (%turn-player nil a)
   (%pending-armies nil a :reader pending-armies)))

(defmethod initialize-instance :after ((game game) &rest rest
                                                   &key game-map players)
  (declare (ignorable rest))
  (bind (((:slots
            (the-game-map game-map)
            (the-players players)
            (the-players-round %players-round)
            (the-turn-player %turn-player)
            (the-fsm game-fsm)
            (the-pending-armies %pending-armies)) game)
          ((:flet _switch (state)) (switch the-fsm state))
          ((:flet _rotate-turn ()) (%rotate-turn game))
          ((:flet _reset-movable-armies ()) (%reset-movable-armies game))
          ((:flet _place-armies (where amount))
           (%place-armies game where amount))
          ((:flet pass-placing (old-armies new-state &optional new-armies))
           (when (plusp the-pending-armies)
             (error "No se pusieron todos los ejércitos"))
           (setf the-pending-armies
                 (if (_rotate-turn)
                   (progn
                     (_switch new-state)
                     new-armies)
                   old-armies)))
          ((:flet _placeable-armies ())
           (placeable-armies (turn-player game)))
          ((:flet _initial-armies ()) (initial-armies game))
          ((:flet 2-initial-armies ()) (* 2 (initial-armies game))))
    (setf the-game-map
          (typecase game-map
            (game-map game-map)
            (t (read-map game-map))))
    (dolist (name players)
      (pushnew (setf (gethash name (%players game))
                     (make-instance 'player :player-name name :game game))
               the-players-round))
    (setf the-players-round (reverse the-players-round))
    (nconc the-players-round the-players-round)
    (setf the-turn-player the-players-round)
    (setf the-fsm
          (make-fsm
            (:claiming
              (:claim ((fsm territory)
                       (%claim game territory)
                       (_rotate-turn)
                       (done game))
               :done ((fsm)
                      (if (every (lambda (k) (owner (territory game k)))
                                 (territory-keys game))
                        (_switch :setup))))
              :setup
              (:done ((fsm)
                      (setf the-pending-armies (2-initial-armies))
                      (_switch :placing-twice-n)))
              :placing-twice-n
              (:place-armies ((fsm where amount)
                              (_place-armies where amount))
               :done ((fsm)
                      (pass-placing (2-initial-armies)
                                    :placing-n (_initial-armies))))
              :placing-n
              (:place-armies ((fsm where amount)
                              (_place-armies where amount))
               :done ((fsm)
                      (pass-placing (_initial-armies) :attacking)
                      (_reset-movable-armies)))
              :attacking
              (:attack ((fsm from to)
                        (%attack game from to))
               :done ((fsm)
                      (_switch :regrouping)))
              :regrouping
              (:move-armies ((fsm from to amount)
                             (%move-armies game from to amount))
               :done ((fsm)
                      (_switch
                        (if (%pass-turn game)
                          (progn
                            (setf the-pending-armies (_placeable-armies))
                            :placing)
                          :attacking))))
              :placing
              (:place-armies ((fsm where amount)
                              (_place-armies where amount))
               :done ((fsm)
                      (when (plusp the-pending-armies)
                        (error "no se pusieron todos los ejércitos"))
                      (setf the-pending-armies
                            (if (_rotate-turn)
                              (progn
                                (_reset-movable-armies)
                                (_switch :attacking)
                                nil)
                              (_placeable-armies))))))
            :claiming))))

(defgeneric read-map (file)
  (:method ((list list))
   (let* ((territories (make-hash-table :test 'equalp))
          (frontiers (make-container 'graph-container))
          (initial-armies (getf list :initial-armies)))
     (dolist (territory (getf list :territories))
       (destructuring-bind (key name extra-armies terr-frontiers) territory
         (add-vertex frontiers key)
         (setf (gethash key territories)
               (make-instance 'territory
                              :territory-name name
                              :extra-armies extra-armies))
         (dolist (frontier-key terr-frontiers)
           (add-edge-between-vertexes frontiers key frontier-key))))
     (make-instance 'game-map
                    :territories territories
                    :frontiers frontiers
                    :initial-armies initial-armies)))

  (:method ((file stream))
   (let ((*read-eval* nil))
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
   (let* ((game (make-instance 'game
                               :game-map (read-map (getf list :game-map))
                               :players (getf list :players)))
          (state (getf list :state))
          (pending-armies (getf list :pending-armies))
          (turn-player (player game (getf list :turn))))
     (loop for (name armies-dist) on (getf list :armies) by #'cddr do
           (loop for (territory-key armies) on armies-dist by #'cddr
                 for territory = (territory game territory-key) do
                 (setf (%owner territory) (player game name))
                 (setf (%armies territory) armies)))
     (when state
       (%reset-movable-armies game)
       (switch (game-fsm game) state)
       (when turn-player
         (loop until (eq (turn-player game) turn-player)
               do (%rotate-turn game)))
       (when pending-armies (setf (%pending-armies game) pending-armies)))
     (unless (or (eq (state game) :claiming)
                 (every (lambda (territory-key)
                          (let ((territory (territory game territory-key)))
                            (and (owner territory)
                                 (plusp (armies territory)))))
                        (territory-keys game)))
       (error "Hay territorios sin inicializar"))
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

(defgeneric territory (container key)
  (:method ((container game-map) key)
   (gethash key (territories container)))
  (:method ((container game) key)
   (territory (game-map container) key)))

(defgeneric head-player (game)
  (:method ((game game))
   (car (%players-round game))))

(defgeneric turn-player (game)
  (:method ((game game))
   (car (%turn-player game))))

(defmethod initial-armies ((game game))
  (cadr (assoc (length (player-keys game))
               (initial-armies (game-map game)))))

(defmethod territories ((game game))
  (territories (game-map game)))

(defmethod frontiers ((game game))
  (frontiers (game-map game)))

(defgeneric player (game player-key)
  (:method ((game game) player-key)
   (gethash player-key (%players game))))

(defgeneric territory-keys (game)
  (:method ((game game))
   (hash-table-keys (territories game))))

(defgeneric player-keys (game)
  (:method ((game game))
   (hash-table-keys (%players game))))

(defmethod state ((game game))
  (state (game-fsm game)))

(defmethod armies ((player player))
  (loop for territory being the hash-values in (territories (game player))
        when (eq (owner territory) player)
        summing (armies territory)))

(defmethod territories ((player player))
  (loop for territory being the hash-values in (territories (game player))
        when (eq (owner territory) player) count it))

(defgeneric placeable-armies (player)
  (:method ((player player))
   (values (ceiling (/ (territories player) 2)))))

(defgeneric territories-connected-p (game origin-key destination-key)
  (:method ((game game) origin-key destination-key)
   (let ((graph (frontiers game)))
     (vertices-share-edge-p
       (find-vertex graph origin-key)
       (find-vertex graph destination-key)))))

(defmethod send ((game game) message &rest rest)
  (apply #'send (game-fsm game) message rest)
  game)

(defmethod claim ((game game) territory)
  (send game :claim territory))

(defmethod done ((game game))
  (send game :done))

(defmethod place-armies ((game game) where amount)
  (send game :place-armies where amount))

(defmethod attack ((game game) from to)
  (send game :attack from to))

(defmethod move-armies ((game game) from to amount)
  (send game :move-armies from to amount))

(defgeneric shuffle-territories (game)
  (:method ((game game))
   (let ((territory-count (size (territories game)))
         (player-count (size (%players game))))
     (when (= territory-count 0)
       (error "El juego debe tener territorios"))
     (when (= player-count 0)
       (error "El juego debe tener jugadores"))
     (unless (= (rem territory-count player-count) 0)
       (error "Los territorios deben ser divisibles entre los jugadores"))
     (loop
       with territories = (shuffle (territory-keys game))
       for player in (%players-round game)
       for territory in territories do
       (setf (%owner (territory game territory)) player)
       (setf (%armies (territory game territory)) 1)))))

(defgeneric %pass-head (game)
  (:method ((game game))
   (pop (%players-round game))
   (head-player game)))

(defgeneric %rotate-turn (game)
  (:method ((game game))
   (pop (%turn-player game))
   (eq (turn-player game) (head-player game))))

(defgeneric %pass-turn (game)
  (:method ((game game))
   (let ((is-head-player (%rotate-turn game)))
     (when is-head-player
       (%pass-head game)
       (%rotate-turn game))
     is-head-player)))

(defgeneric %claim (game territory)
  (:method ((game game) territory-key)
   (let ((territory (territory game territory-key)))
     (when (owner territory)
       (error "El territorio ya tiene dueño"))
     (setf (%owner territory) (turn-player game))
     (setf (%armies territory) 1))))

(defgeneric %reset-movable-armies (game)
  (:method ((game game))
   (loop for territory being the hash-values in (territories game) do
         (setf (%movable-armies territory) (armies territory)))))

(defgeneric %move-armies (game from to amount)
  (:method ((game game) origin-key destination-key amount)
   (let ((origin (territory game origin-key))
         (destination (territory game destination-key)))
     (unless (eq (owner origin) (turn-player game))
       (error "Los territorios deben pertenecer al jugador de turno"))
     (unless (territories-connected-p game origin-key destination-key)
       (error "Los territorios deben estar conectados"))
     (unless (eq (owner origin) (owner destination))
       (error "Los territorios tienen que tener el mismo dueño"))
     (unless (plusp amount)
       (error "No se pueden mover 0 o menos ejércitos"))
     (unless (>= (movable-armies origin) amount)
       (error "No hay suficientes ejércitos"))
     (decf (%movable-armies origin) amount)
     (decf (%armies origin) amount)
     (incf (%armies destination) amount))))

(defgeneric %place-armies (game where amount)
  (:method ((game game) territory-key (amount integer))
   (let ((territory (territory game territory-key)))
     (unless (eq (owner territory) (turn-player game))
       (error "El territorio debe pertenecer al jugador de turno"))
     (unless (plusp amount)
       (error "No se pueden poner 0 o menos ejércitos"))
     (unless (<= amount (pending-armies game))
       (error "No quedan tantos ejércitos por poner"))
     (decf (%pending-armies game) amount)
     (incf (%armies territory) amount))))

(defgeneric %attack (game from to)
  (:method ((game game) origin-key destination-key)
   (symbol-macrolet
     ((origin (territory game origin-key))
      (destination (territory game destination-key))
      (attackers (%armies origin))
      (defenders (%armies destination)))
     (unless (eq (owner origin) (turn-player game))
       (error "El territorio origen debe pertenecer al jugador de turno"))
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
         (setf (%owner destination) (owner origin))
         (%move-armies game origin-key destination-key 1)))
     (eq (owner origin) (owner destination)))))

(defmethod print-object ((game game) stream)
  (format stream "~&Jugadores:~t~{~a~^, ~}"
          (loop repeat (size (%players game))
                for p in (%players-round game)
                for n = (player-name p)
                collecting (if (equal p (turn-player game))
                             (format nil "-~a-" n)
                             n)))
  (format stream "~&Etapa:~t~a" (state game))
  (if (pending-armies game)
    (format stream "~&Ejércitos pendientes:~t~a" (pending-armies game)))
  (format stream "~&Territorios:")
  (loop for v being the hash-values in (territories game)
        using (hash-key k) do
        (format stream "~& ~a~23t~a~37tDueño: ~a~55tEjércitos: ~a"
                (territory-name v) k
                (if (owner v) (player-name (owner v)) "-")
                (if (armies v) (armies v) "-"))))

