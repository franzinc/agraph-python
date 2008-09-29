(asdf:defsystem :agraph-http-client
  :depends-on (:st-json)
  :components
  ((:module "client" :components
            ((:file "packages")
             (:file "misc" :depends-on ("packages"))
             (:file "request" :depends-on ("misc"))
             (:file "client" :depends-on ("request" "misc"))))))
