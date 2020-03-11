(defpackage :chatserver
  (:use :cl :iolib)
  (:export :start-server))
(in-package :chatserver)

(defun start-server(&key (port 29902))
  (iolib:with-open-socket
      (server :connect :passive
		    :address-family :internet
		    :type :stream
		    :ipv6 nil
		    :external-format '(:utf-8 :eol-style :crlf))
    (format t "Created socket~%")
    (iolib:bind-address server iolib:+ipv4-unspecified+ :port port :reuse-addr t)
    (format t "Bound socket~%")
    (iolib:listen-on server :backlog 5)
    (format t "Listening on socket~%")
    (loop
       (format t "Waiting to accept a connection...~%")
       (iolib:with-accept-connection (client server :wait t)
	 (multiple-value-bind (who rport)
	     (iolib:remote-name client)
	   (format t "Got a connection from ~A:~A!~%" who rport))
	 (multiple-value-bind (s m h d mon y)
	     (get-decoded-time)
	   (format t "Sending the time...")
	   (handler-case
	       (progn
		 (format client "~A/~A/~A ~A:~A:~A~%" mon d y h m s)
		 (finish-output client))
	     (iolib:socket-connection-reset-error ()
	       (format t "Client reset connection~%"))
	     (iolib:hangup ()
	       (format t "Client closed connection~%")))
	   (format t "Sent~%"))))
    t))
