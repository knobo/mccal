;;;; mccal.asd

(asdf:defsystem #:mccal
  :description "Describe mccal here"
  :author "Knut Olav Bøhmer"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria"
               "mcclim"
               "local-time"
               "local-time-duration")
  :components ((:file "package")
               (:file "mccal")
               (:file "calendar")))
