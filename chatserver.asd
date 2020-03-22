(defsystem :chatserver
  :version "0.1.0"
  :depends-on (:chatserver/server :chatserver/test-client))

(defsystem :chatserver/server
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:usocket :bordeaux-threads)
  :pathname "src/server/"
  :components ((:file "main"))
  :description "")

(defsystem :chatserver/test-client
  :version "0.1.0"
  :depends-on (:usocket :bordeaux-threads :chatserver/server)
  :pathname "src/test-client/lisp/"
  :components ((:file "main"))
  :description "Test client")
  
