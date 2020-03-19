(defsystem :chatserver/test-client
  :version "0.1.0"
  :depends-on (:usocket)
  :pathname "src/test-client/lisp/"
  :components ((:file "main"))
  :description "Test client")

(defsystem :chatserver
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:usocket :chatserver/test-client)
  :pathname "src/server/"
  :components ((:file "main"))
  :description "")
  
