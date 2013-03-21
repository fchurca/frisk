(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp :cl-graph :cl-containers :metatilities :metabang-bind)
  (:export :territory
           :name
           :extra-armies
           :owner
           :armies
           :player
           :read-map
           :move-armies
           :place-armies
           :attack
           :game
           :territories
           :territory-keys
           :frontiers
           :turn-player
           :territories-connected-p))

