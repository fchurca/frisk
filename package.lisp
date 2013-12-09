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
           :game
           :read-map
           :read-game
           :territory-keys
           :player-keys
           :territories-connected-p
           :add-player
           :shuffle-territories
           :move-armies
           :place-armies
           :attack
           :print-game))

