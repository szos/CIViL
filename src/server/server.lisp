;;;; server.lisp

(in-package :civ-server)

(defparameter *introduction-server*
  (bt:make-thread 
   (lambda ()
     (usocket:))))

(defun my-echo-server ()
  (format t "starting server~%")
  (as:tcp-server "127.0.0.1"
		 9901
		 (lambda (s d)
		   (declare (ignorable s d))
		   (format t "~a, ~a" s d)
		   (as:write-socket-data s d))
		 :event-cb (lambda (err) (format t "listener event: ~a~%" err)))
  (as:signal-handler 2 (lambda (sig)
			 (declare (ignore sig))
			 (as:exit-event-loop))))

(as::with-event-loop )
(as:start-event-loop
 (lambda ()
   (as:tcp-connect "127.0.0.1" 9901
		   (lambda (s b) (format t "~a ~a" s b))
		   (lambda (event) (format t "event: ~a ~%" event)))))

(defun socket-read (connection)
  (loop for message = (read connection)
	while message
	do (format t "~a" message)))

(defun socket-event (ev)
  (format t "socket event: ~a" ev)) 

(defun connect (server port)
  (let ((stream (as:tcp-connect server port
				(lambda (socket stream)
				  (declare (ignorable socket stream))
				  (loop for message = (read stream)
					while message
					do (format t "~a" message)))
				#'socket-event
				:stream t
				:data "hi there")))
    stream))



(defun connection-socket-read (socket stream)
  (format t "We should read the IRC message from ~a ~%" stream))

(defun connection-socket-event (ev)
  (format t "Socket event: ~a~%" ev))

(defun connect (server &optional (port 6667))
  (as:tcp-connect server port
		  #'connection-socket-read
		  :event-cb #'connection-socket-event
		  :stream t
		  :data "hi"))

(as:start-event-loop (lambda () (connect "127.0.0.1" 9901)))
