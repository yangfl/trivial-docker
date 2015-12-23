(in-package :trivial-docker)

(defdocker container-list ()
  (write-http-get stream "/containers/json?all=1"))

(defdocker container-create (parameters)
  (write-http-post stream "/containers/create" +mime-json+ (encode-json-to-string parameters)))

(defdocker container-inspect (id)
  (write-http-get stream (format nil "/containers/~a/json" id)))

(defdocker container-top (id)
  (write-http-get stream (format nil "/containers/~a/top?ps_args=aux" id)))

(defdocker container-start (id)
  (write-http-post stream (format nil "/containers/~a/start" id)))

(defdocker container-stop (id)
  (write-http-post stream (format nil "/containers/~a/stop" id)))

(defdocker container-pause (id)
  (write-http-post stream (format nil "/containers/~a/pause" id)))

(defdocker container-unpause (id)
  (write-http-post stream (format nil "/containers/~a/unpause" id)))

(defun container-attach (socket id)
  (let* ((socket (docker-connect (get-peer-address socket) (get-peer-port socket) nil))
         (stream (socket-stream socket)))
    (write-http-post
      stream (format nil "/containers/~a/attach?logs=0&stream=1&stdin=1&stdout=1&stderr=1" id))
    (list
      (response-read-code stream)
      (response-read-headers stream)
      socket)))

(defdocker container-remove (id)
  (write-http-delete stream (format nil "/containers/~A?v=1&force=1" id)))
