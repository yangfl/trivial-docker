(defpackage :trivial-docker
  (:import-from :cl-json
    :encode-json-to-string
    :decode-json
    :decode-json-from-string)
  (:import-from :usocket
    :with-server-socket
    :socket-close
    :socket-connect
    :socket-stream
    :get-peer-address
    :get-peer-port)
  (:use :common-lisp)
  (:export
    :with-server-socket
	:socket-stream
    :socket-close

    :docker-connect

    :container-list
    :container-create
    :container-inspect
    :container-top
    :container-start
    :container-pause
    :container-unpause
    :container-attach
    :container-remove

    :docker-auth
    :docker-info
    :docker-version
    :docker-ping
    :docker-events))
