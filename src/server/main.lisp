(defpackage :chatserver
  (:use :cl :usocket)
  (:export :start-server))
(in-package :chatserver)

(defun start-server (&key (port 29902))
  (let ((listen-socket (usocket:socket-listen nil port)))
    (let ((socket (usocket:socket-accept listen-socket)))
      (format t "~A~%" "Connected")
      (let ((client-sockets (usocket:wait-for-input socket :ready-only t :timeout 10)))
        (format t "Client says: ~A~%" (read-line (usocket:socket-stream (first client-sockets)))))
      (usocket:socket-close socket)
      (usocket:socket-close listen-socket))))
