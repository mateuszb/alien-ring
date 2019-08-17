;;;; alien-ring.asd

(asdf:defsystem #:alien-ring
  :description "Describe alien-ring here"
  :author "Mateusz Berezecki"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("cffi")
  :components ((:file "package")
               (:file "alien-ring")))
