(asdf:defsystem :agraph-http-tests
  :depends-on (:agraph-http-server :agraph-http-client)
  :components
  ((:module "test" :components
            ((:file "package")
             (:file "util" :depends-on ("package"))             
             (:file "test" :depends-on ("util"))))))
