(asdf:defsystem :agraph-http-server
  :depends-on (:st-json)
  :components
  ((:module "client" :components
            ((:file "packages")
             (:file "request" :depends-on ("packages"))
             (:file "client" :depends-on ("request"))))))
