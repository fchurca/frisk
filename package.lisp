(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp
        :cl-graph
        :cl-containers
        :metabang-bind)
  (:import-from :metatilities :defclass*)
  (:import-from :alexandria :shuffle :hash-table-keys :doplist)
  (:export
    ; FSM
    :fsm
    :make-fsm
    :switch
    :states
    :available-messages
    :send
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
    :claim
    :done
    :place-armies
    :attack
    :move-armies
    :territory-keys ; These two are
    :player-keys    ; kinda-accessor-ish 
    :territories-connected-p
    :shuffle-territories
    :print-game))

