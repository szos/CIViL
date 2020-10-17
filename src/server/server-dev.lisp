
(defmacro make-thread-closure ((&rest argslist) &body body)
  `(let ,(loop for arg in argslist
	       if (listp arg)
		 collect arg
	       else
		 collect (list arg arg))
     (bt:make-thread
      (lambda ()
	,@body))))

(make-thread-closure (*standard-output*)
  (format t "hi"))

(defun threadify (function &rest args)
  (bt:make-thread
   (lambda ()
     (apply function args))))

;; (threadify 'start-server "127.0.0.1" 4001)

(defun make-server (host port)
  (format ))


(defun make-server (host port)
  (let ((server-host host)
	(server-port port)
	(stdout *standard-output*))
    (bt:make-thread
     (lambda ()
       (format stdout "starting server on ~a:~a" server-host server-port)
       (usocket:with-server-socket (server-socket (usocket:socket-listen server-host server-port))
	 (let ((conn (usocket:socket-accept server-socket :element-type 'character)))
	   (unwind-protect
		(loop for line = (read-line (usocket:socket-stream conn))
		      until (string= line "exit")
		      do (format stdout "receivd: ~a~%" line))
	     (usocket:socket-close conn)))))
     :name "command-line thread")))

(defun string-to-symbols (string)
  (let ((stream (make-string-input-stream string)))
    (loop for symbol = (read stream nil)
	  until (not symbol)
	  collect symbol)))

;;;;;;; (move (to (variable)) (from (variable)) (with (variable)))
;; (move (:unit x v w))
;;; and we have
;;; "move unit2951 +x 1 +w 1 -v 2"

;; (defun parse-move (unit list)
;;   (case (car list)
;;     (x
;;      (move-unit unit :x (cadr list))
;;      (parse-move unit (cddr list)))
;;     (w
;;      (move-unit unit :w (cadr list))
;;      (parse-move unit (cddr list)))
;;     (v
;;      (move-unit unit :v (cadr list))
;;      (parse-move unit (cddr list)))))

(defun parse-connection (line)
  (let ((l (string-to-symbols line)))
    (case (car l)
      ;;(move (parse-move (cadr l) (cddr l)))
      (exit
       ))))

(defun make-handshake-server (host port)
  (let ((server-host host)
	(server-port port)
	(stdout *standard-output*))
    (bt:make-thread
     (lambda ()
       (format stdout "starting handshake server on ~a:~a" server-host server-port)
       (usocket:with-server-socket (server-socket (usocket:socket-listen server-host server-port))
	 (let ((conn (usocket:socket-accept server-socket :element-type 'character)))
	   (unwind-protect
		(loop for line = (read-line (usocket:socket-stream conn))
		      until (string= line "exit")
		      do (format stdout "receivd: ~a~%" line)
			 (parse-connection line))
	     (usocket:socket-close conn)))))
     :name "handshake server")
    (bt:make-thread
     (lambda ()
       (format "starting internal communique server ~a:~a" )))))
