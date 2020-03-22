(defpackage :chatserver
  (:use :cl :usocket :bordeaux-threads) ;bt is shorthand for bordeaux-threads
  (:export :start-server :say))
(in-package :chatserver)

(defmacro push-front (obj place)
  (let ((obj-name (gensym)) (place-name (gensym)))
    `(let ((,obj-name (if (typep ,obj 'cons) ,obj (cons ,obj nil)))
	     (,place-name ,place))
       (if ,place-name
	   (setf (rest (last ,place-name)) ,obj-name)
	   (setf ,place-name ,obj-name)))))

(defmacro pairlis-alt (bindlist)
  (let ((first-list-name (gensym)) (second-list-name (gensym)))
    `(let ((,first-list-name (list)) (,second-list-name (list)))
       (dolist (item ,bindlist)
	  (push (first item) ,first-list-name)
	  (push (second item) ,second-list-name))
       (pairlis ,first-list-name ,second-list-name))))

;;;This isn't so fun to use at the moment, improve later
(defmacro start-doing-concurrently (fun (&rest bindings-list))
  `(let ((bindings (pairlis-alt ,bindings-list)))
    ;(format t "~A~%" bindings)
     (let ((bt:*default-special-bindings* bindings))
       (bt:make-thread ,fun))))

(defun prompt ()
  (format t "Listen?~%")
  (read-line))

(define-condition sending-message-error (error)
  ((retries :initarg :retries :reader retries)))

;;;Sends a msg to a socket
(defun say (socket msg &optional (retries 0))
  (restart-case (handler-case (progn
				(format (usocket:socket-stream socket) (concatenate 'string msg "~%"))
				(force-output (usocket:socket-stream socket)))
		  (error () (error 'sending-message-error :retries retries))) ;Wrap whatever error occured
    (retry () (say socket msg (+ retries 1)))
    (give-up () nil)))

;;;Sends a msg to the sockets (list with t as the first element) specified
(defun say-multiple(sockets msg)
  (dolist (socket (rest sockets)) (funcall #'say socket msg)))

;;;This is a destructive function, it modifies the "sockets" list
;;;Expects port, sockets (list with t as the first element), sockets-lock,
;;;listen-state (cons cell), listen-state-lock to be bound
(defun listen-for-new-connections ()
  (format t "Listening.~%")
  (let ((listen-socket (usocket:socket-listen "localhost" port :backlog 10)))
    (do ()
	((bt:with-lock-held (listen-state-lock) (not (first listen-state)))
	 (usocket:socket-close listen-socket)
	 (format t "Closing the listening socket.~%"))
      ;(format t "Accepting...~%")
      (let ((ready-socket (usocket:wait-for-input listen-socket :ready-only t :timeout 0.5)))
	(when ready-socket
	  (let ((new-socket (handler-case (usocket:socket-accept listen-socket)
			      (error (e)
				(format t "Couldn't accept a new connection for some reason.~% ~A~%" e)
				nil))))
	    (when new-socket
	      (format t "Adding a new socket to the sockets list.~%")
	      (bt:with-lock-held (sockets-lock)
		(format t "~A~%" (push-front new-socket sockets))))))))))

(define-condition generic-socket-error (error)
  ())

(defun handle-sending-message-error (e)
  (if (< (retries e) 3)
      (invoke-restart 'retry)
      (error 'generic-socket-error)))

(defun handle-ready-socket (ready-socket sockets)
  (let ((msg (handler-case (read-line (usocket:socket-stream ready-socket))
	       (end-of-file () (format t "A client closed the socket stream.~%")
			    (error 'generic-socket-error)))))
    (when (> (length msg) 0)
      (handler-bind ((sending-message-error #'handle-sending-message-error))
	(say-multiple sockets msg)))))

;;;sockets has t as the first element
(defun sockets-close (sockets)
  (format t "Closing some sockets...~%")
  (dolist (socket (rest sockets)) (usocket:socket-close socket)))

;;;This is a destructive function, it modifies the "sockets" list
;;;Expects sockets (list with t as the first element), sockets-lock,
;;;handle-state (cons cell), handle-state-lock to be bound
(defun handle-sockets ()
  (format t "Handling sockets...~%")
  (do ()
      ((bt:with-lock-held (handle-state-lock) (not (first handle-state)))
       (bt:with-lock-held (sockets-lock) (sockets-close sockets)))
    ;(sleep 0.1)
    (when (bt:with-lock-held (sockets-lock) (rest sockets))
      ;(format t "Waiting for input...~%")
      (let ((ready-sockets (usocket:wait-for-input (bt:with-lock-held (sockets-lock)
						     (rest sockets)) :ready-only t :timeout 0.5)))
	(dolist (ready-socket ready-sockets) (handler-case
						 (handle-ready-socket ready-socket sockets)
					       (generic-socket-error () (format t "Deleting socket~%")
								     (bt:with-lock-held (sockets-lock)
								       (usocket:socket-close ready-socket)
								       (delete ready-socket sockets)))))))))

;;;This is a destructive function, it modifies the "sockets" list
(defun start-listening-for-new-connections-concurrently (port sockets sockets-lock listen-state listen-state-lock)
  (start-doing-concurrently #'listen-for-new-connections `((port ,port)
							   (sockets ,`(values ',sockets))
							   (sockets-lock ,sockets-lock)
							   (listen-state ,`(values ',listen-state))
							   (listen-state-lock ,listen-state-lock))))

;;;This is a destructive function, it modifies the "sockets" list
(defun start-handling-sockets-concurrently (sockets sockets-lock handle-state handle-state-lock)
  (start-doing-concurrently #'handle-sockets `((sockets ,`(values ',sockets))
					       (sockets-lock ,sockets-lock)
					       (handle-state ,`(values ',handle-state))
					       (handle-state-lock ,handle-state-lock))))

(defun handle-io (quit-state quit-state-lock)
  (bt:with-lock-held (quit-state-lock)
    (check-type quit-state cons))
  (do (in)
      ((equalp in "quit") (bt:with-lock-held (quit-state-lock)
			    (setf (first quit-state) nil)))
       (setf in (read-line))))
  
(define-condition invalid-port-error (error)
  ())

(defun check-port (port)
  (handler-case (check-type port integer)
    (check-type-error () (error 'invalid-port-error)))
  (when (or (< port 0) (> port 65535))
    (error 'invalid-port-error)))

(defun start-server (&key (port 29902))
  (handler-case
      (progn
	(check-port port)
	(let ((sockets (list t)) (quit-state (cons t nil))
	      (sockets-lock (bt:make-lock)) (quit-state-lock (bt:make-lock))
	      (listen-thread nil))
	  (start-listening-for-new-connections-concurrently port sockets sockets-lock quit-state quit-state-lock)
	  (start-handling-sockets-concurrently sockets sockets-lock quit-state quit-state-lock)
	  (format t "Server running on port ~D.~%" port)
	  (handle-io quit-state quit-state-lock)
	  (sleep 1)))
    (invalid-port-error () (format t "Invalid port.~%") nil)
    (error (e) (format t "Encountered an unknown error.~%")
	   (format t "Error message: ~A" e))))

    
    
    
    
