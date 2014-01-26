(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp
        :cl-graph
        :cl-containers)
  (:import-from :metatilities :defclass*)
  (:import-from :alexandria :shuffle :hash-table-keys)
  (:export
    ; FSM
    :fsm
    :make-fsm
    :switch
    :states
    :available-messages
    ; Classes
    :territory
    :game-map
    :player
    :game
    ; Accessors
    :player-name
    :territory-name
    :extra-armies
    :owner
    :armies
    :territories
    :frontiers
    :players
    ; Functions and methods and such, oh my!
    :read-map
    :read-game
    :territory-keys ; These two are
    :player-keys    ; kinda-accessor-ish 
    :territories-connected-p
    :add-player
    :shuffle-territories
    :move-armies
    :place-armies
    :attack
    :print-game))

