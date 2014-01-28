(defsystem frisk
  :name "frisk"
  :author
  "Federico Churca-Torrusio <federico.churca@gmail.com>;
   Javier Zaniratto <javizaniratto@gmail.com>"
  :license "BSD"
  :description "Implementation of a Risk-like board game"
  :long-description ""
  :components ((:file "fsm") 
               (:file "package" :depends-on ("fsm"))
               (:file "frisk" :depends-on ("package")))
  :depends-on (:alexandria :cl-graph :metatilities-base :iterate))

