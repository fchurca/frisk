(defpackage :ar.com.fchurca.frisk
  (:use :common-lisp
        :cl-graph
        :cl-containers
        :metabang-bind)
  (:import-from :metatilities :defclass*)
  (:import-from :alexandria :shuffle :hash-table-keys)
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

