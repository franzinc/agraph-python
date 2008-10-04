(asdf:defsystem :agraph-http-server
  :depends-on (:salza2 :st-json)
  :components
  ((:module "src" :components
            ((:file "packages")
             (:file "misc" :depends-on ("packages"))
             (:file "cursor" :depends-on ("packages"))
             (:file "prolog" :depends-on ("packages"))
             (:file "deflate" :depends-on ("packages"))
             (:file "formats" :depends-on ("misc" "cursor"))
             (:file "server" :depends-on ("misc"))
             (:file "service" :depends-on ("misc"))
             (:file "services" :depends-on ("service" "server" "cursor" "prolog"))
             (:file "http" :depends-on ("service" "server" "formats" "deflate"))))))
