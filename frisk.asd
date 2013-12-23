(defsystem frisk
  :name "frisk"
  :author
  "Federico Churca-Torrusio <federico.churca@gmail.com>;
   Javier Zaniratto <javizaniratto@gmail.com>"
  :license "BSD"
  :description "Implementation of a Risk-like board game"
  :long-description ""
  :components ((:file "package")
               (:file "fsm" :depends-on ("package")) 
               (:file "frisk" :depends-on ("package" "fsm")))
  :depends-on (:alexandria :cl-graph :metatilities-base))

