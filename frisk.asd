(defsystem frisk
  :name "frisk"
  :author
  "Federico Churca-Torrusio <federico.churca@gmail.com>;
   Javier Zaniratto <javizaniratto@gmail.com>"
  :license "BSD"
  :description "Implementation of a Risk-like board game"
  :long-description ""
  :components ((:file "package")
               (:file "frisk" :depends-on ("package")))
  :depends-on (:cl-graph :metatilities-base))

