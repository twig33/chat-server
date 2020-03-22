(defpackage :chatserver/test-client
  (:use :cl :usocket :bordeaux-threads) ; bt is shorthand for bordeaux-threads
  (:export :start-client))
(in-package :chatserver/test-client)

(define-condition server-connection-error (error)
  ())

(define-condition quit-condition ()
  ())

(defun stop-client (socket)
  (format t "Quitting.~%")
  (when socket
    (usocket:socket-close socket))
  t)

(defun prompt-host-and-port ()
  (let (host port)
    (format t "Host: ")
    (setf host (read-line))
    (format t "Port: ")
    (setf port (read))
    (values host port)))

(defun connect (host port)
  (format t "Connecting to the server...~%")
  ;;Wrap all usocket errors in server-connection-error (and provide restarts)
  (restart-case (handler-case (usocket:socket-connect host port)
		  (error () (error 'server-connection-error)))
    (retry () (connect host port))
    (use-host-port (host port) (connect host port))))
  
;(defun say (socket msg)
;  (format (usocket:socket-stream socket) (concatenate 'string msg "~%"))
;  (force-output (usocket:socket-stream socket)))

(defun say-hello (socket)
  (chatserver:say socket "Hello server"))

;;; Expects socket, socket-lock, listen-state (cons cell), listen-state-lock to be bound
(defun listen-to-server ()
  #||  (format t "Server listener~%")
  (do ()
      ((not (first (bt:with-lock-held (listen-state-lock) listen-state))))
    (bt:acquire-lock socket-lock)
    (when (listen (usocket:socket-stream socket))
      (format t "Server says: ~A~%" (read-line (usocket:socket-stream socket))))
    (bt:release-lock socket-lock)
    (sleep 0.5))
  (format t "Server listener signing off~%")) ||#
  (listen-to-server-alt))

;;; Expects socket, socket-lock, listen-state (cons cell), listen-state-lock to be bound
(defun listen-to-server-alt ()
  (format t "Server listener~%")
  (do ()
      ((not (first (bt:with-lock-held (listen-state-lock) listen-state))))
    (let ((ready-socket (first (usocket:wait-for-input socket :ready-only t)))) ;Assume wait-for-input thread safety
      (format t "Server says: ~A~%" (read-line (usocket:socket-stream socket)))))
  (format t "Server listener signing off~%"))

;;; called from the Main thread
(defun communicate (socket)
  (let ((listen-state (cons t nil))
	(socket-lock (bt:make-lock)) (listen-state-lock (bt:make-lock))
	(listen-thread nil))
    (let  ((bt:*default-special-bindings*
	    (pairlis '(socket socket-lock listen-state listen-state-lock)
		     (list socket socket-lock `(values ',listen-state) listen-state-lock))))
      (setf listen-thread (bt:make-thread #'listen-to-server :name "server msgs listen thread")))
    (format t "Ready to chat with the server.~%")
    (format t "To quit, input \"quit\".~%")
    (format t "To send a message to the server, input anything else.~%")
    (do (in)
	((equalp in "quit"))
      (setf in (read-line))
      (bt:with-lock-held (socket-lock)
	(chatserver:say socket in)))
    (bt:with-lock-held (listen-state-lock)
      (setf (first listen-state) nil))
    (sleep 1))
  (format t "Finished chatting with the server.~%"))
  
(defun handle-server-connection-error (e)
  (format t "Couldn't connect to the server.
Input \"retry\" to retry.
Input \"edit-server-info\" to input new host address and port.
Input anything else to quit.~%")
  (let ((in (read-line)))
    (cond ((equalp "retry" in) (invoke-restart 'retry))
	   ((equalp "edit-server-info" in) (multiple-value-bind (host port) (prompt-host-and-port)
				  (invoke-restart 'use-host-port host port)))
	  (t (signal 'quit-condition)))))

(defun start-client (&key (host "localhost") (port 29902))
  (let ((socket nil))
    (handler-case
	(progn
	  (setf socket (handler-bind ((server-connection-error #'handle-server-connection-error))
			 (connect host port)))
	  (communicate socket)
	  (signal 'quit-condition))
      (quit-condition () (stop-client socket)))))

;;;Built on top of test-client
(defun start-server (&key (host "localhost") (port 29902))
  (let ((listen-socket (usocket:socket-listen "localhost" 29902)))
    (let ((socket (usocket:socket-accept listen-socket)))
      (communicate socket)
      (usocket:socket-close socket))))
