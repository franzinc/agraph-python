(require :asdf)

(load (merge-pathnames #p"load.lisp" *load-pathname*))
(asdf:oos 'asdf:load-op :agraph-http-tests)
