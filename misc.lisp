(in-package :trivial-docker)

(defdocker docker-auth (parameters)
  (write-http-post stream "/auth" +mime-json+ (encode-json-to-string parameters)))

(defdocker docker-info ()
  (write-http-get stream "/info"))

(defdocker docker-version ()
  (write-http-get stream "/version"))

(defun docker-ping (socket &optional on-error)
  (let ((stream (socket-stream socket)))
    (write-http-get stream "/_ping")
    (let ((response (docker-response-read stream)))
      (if on-error
        (assert (string= (third response) "OK")))
      response)))

(defdocker docker-events ()
  (write-http-get stream "/events"))
