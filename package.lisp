(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp
        :cl-graph
        :cl-containers
        :metabang-bind
        :ar.com.fchurca.fsm)
  (:import-from :metatilities :defclass*)
  (:import-from :alexandria :shuffle :hash-table-keys :doplist)
  (:export
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
    :send
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

