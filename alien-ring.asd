;;;; alien-ring.asd

(asdf:defsystem #:alien-ring
  :description "Describe alien-ring here"
  :author "Mateusz Berezecki"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on ("cffi"
	       "trivial-gray-streams")
  :components ((:file "package")
               (:file "alien-ring" :depends-on ("package"))
	       (:file "streams" :depends-on ("alien-ring"))))
