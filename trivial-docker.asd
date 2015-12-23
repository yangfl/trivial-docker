(defsystem trivial-docker
  :name "trivial-docker"
  :author "Yangfl"
  :description "Docker api for Common Lisp."
  :license "public domain"
  :version (:read-file-form "version.lisp-expr")
  :depends-on (:cl-json :usocket)
  :components (
    (:file "package")
    (:file "http" :depends-on ("package"))
    (:file "containers" :depends-on ("http"))
    (:file "misc" :depends-on ("http"))))
