;;;; client.asd

(asdf:defsystem #:civ-client
    :description "Client logic for civ"
    :author "Your Name <your.name@example.com>"
    :license  "Specify license here"
    :version "0.0.1"
    :serial t
    :depends-on (:alexandria
		 :cl-ppcre
		 :bt-semaphore)
    :components ((:file "package")
		 (:file "client")))
