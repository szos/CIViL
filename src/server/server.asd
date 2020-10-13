;;;; server.asd

(asdf:defsystem #:civ-server
  :description "server logic for civ"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
	       :cl-ppcre
	       :bt-semaphore
	       :usocket
	       :cl-async
	       :cl-async-ssl)
  :components ((:file "package")
	       (:file "server")))
