(defpackage :chatserver/test-client
  (:use :cl :usocket)
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
  

(defun say-hello (socket)
  (format (usocket:socket-stream socket) "Hello server")
  (force-output (usocket:socket-stream socket)))

(defun communicate (socket)
  (format t "Ready to chat with the server.~%")
  (format t "To quit, input \"quit\".~%"))
  
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
	  (say-hello socket)
	  (signal 'quit-condition))
      (quit-condition () (stop-client socket)))))


