(require :asdf)

(let ((root *load-pathname*))
  (push root asdf:*central-registry*)
  (push (merge-pathnames #p"lib/salza2/" root) asdf:*central-registry*)
  (push (merge-pathnames #p"lib/st-json/" root) asdf:*central-registry*)
  (asdf:oos 'asdf:load-op :agraph-http-server))
