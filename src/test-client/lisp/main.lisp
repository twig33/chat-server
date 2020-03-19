(defpackage :chatserver/test-client
  (:use :cl :usocket)
  (:export :start-client))
(in-package :chatserver/test-client)

(defun start-client (&key (host "localhost") (port 29902))
  (let ((socket (usocket:socket-connect host port)))
    (format t "~A~%" "Connected")
    (format (usocket:socket-stream socket) "Hello server")
    (force-output (usocket:socket-stream socket))
    (usocket:socket-close socket)))
