(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp :cl-graph :cl-containers)
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
           :frontiers
           :turn-player
           :territories-connected-p))
