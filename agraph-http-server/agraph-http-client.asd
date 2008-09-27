(asdf:defsystem :agraph-http-client
  :depends-on (:st-json)
  :components
  ((:module "client" :components
            ((:file "packages")
             (:file "misc" :depends-on ("packages"))
             (:file "request" :depends-on ("packages"))
             (:file "client" :depends-on ("request" "misc"))))))
