;;;; kimpy.asd

(asdf:defsystem #:kimpy
  :description "The hit game"
  :author "Ornyx - TheOrnyx"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sketch #:alexandria)
  :components ((:file "package")
               (:file "kimpy")
	       (:file "player")))
