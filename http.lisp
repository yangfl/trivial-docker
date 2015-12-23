(in-package :trivial-docker)

;;; error ;;;

(define-condition docker-error (error)
  ((header
    :initarg :header)
   (content
    :initarg :content)))

(defmethod print-object ((object docker-error) stream)
  (format stream "~a: ~a" 'docker-error (slot-value object 'content)))

;;; writer ;;;

(defun write-all (str stream)
  (write-line str stream)
  (force-output stream)
  str)

(defconstant +mime-json+ "application/json")
(defconstant +raw-stream-json+ "application/vnd.docker.raw-stream")

;; this next method stolen from Araneida
(defun url-reserved-character-p (c)
  (not (or
    (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\))) (alphanumericp c))))

(defun escape-url-query (query)
  "Escapes a query string in accordance with the HTTP specification."
  (apply #'concatenate 'string
  (loop for c across query
    if (url-reserved-character-p c)
    collect (format nil "%~2,'0X" (char-code c))
    else
    collect (string c))))

(defun write-newline (stream)
  (write-char (code-char 10) stream))

(defmacro defhttpmethod (name args &body body)
  `(defun
      ,(intern (concatenate 'string "WRITE-HTTP-" (symbol-name name)))
      ,(cons 'stream (cons 'url args))
    ,`(write-string ,(concatenate 'string (symbol-name name) " ") stream)
    (write-string url stream)
    (write-string " HTTP/1.1" stream)
    (write-newline stream)
    ,@body
    (write-newline stream)
    (force-output stream)))

(defhttpmethod get ()
  )

(defhttpmethod post (&optional content-type content)
  (when content-type
    (write-string "Content-Type: " stream)
    (write-string content-type stream)
    (write-newline stream)
    (write-string "Content-Length: " stream)
    (format stream "~a" (length content))
    (write-newline stream)
    (write-newline stream)
    (write-string content stream)))

(defhttpmethod delete ()
  )

;;; reader ;;;

(defun read-all (stream &optional wait-p)
  (loop
    with content = (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t)
    for c = (if wait-p
      (let ((c (read-char stream)))
        (setf wait-p nil)
        c)
      (read-char-no-hang stream))
    while c
    do (vector-push-extend c content)
    finally (return content)))

(defun response-read-code (stream)
  (let* ((l (read-line stream))
         (space (position #\Space l)))
    (if space
      (parse-integer l :start space :junk-allowed t)
      0)))

(defun response-read-headers (stream)
  (loop
    for line = (read-line stream)
    until (or
      (eql (length line) 0)
      (eql (elt line 0) (code-char 10))
      (eql (elt line 0) (code-char 13)))
    collect (let* ((line (string-trim `(#\Space ,(code-char 13) ,(code-char 10)) line))
                   (sep-index (position #\: line))
                   (field-name (intern
                     (string-upcase (if sep-index (subseq line 0 sep-index) line)) :keyword))
                   (value (if sep-index (subseq line (+ sep-index 2)))))
      (cons field-name value))))

(defun response-read-content (stream &optional chunked-p)
  (if chunked-p
    (loop
      with content = (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t)
      for chunke-length = (parse-integer (read-line stream) :radix 16)
      while (plusp chunke-length)
      do (progn
        (dotimes (i chunke-length) (vector-push-extend (read-char stream) content))
        (read-line stream))
      finally (progn (read-all stream) (return content)))
    (read-all stream)))

(defun docker-response-read (stream &optional (on-error t))
  (let* ((code (response-read-code stream))
         (header (response-read-headers stream))
         (chunked-p (string= (cdr (assoc :Transfer-Encoding header)) "chunked"))
         (json-p (let ((mime (cdr (assoc :Content-Type header))))
           (if (>= (length mime) 16)
             (string= (subseq mime 0 16) +mime-json+))))
         (content (let ((content (response-read-content stream chunked-p)))
           (if json-p (decode-json-from-string content) content))))
    (if (and on-error (eql code 500))
      (error 'docker-error
        :header header
        :content (string-trim (list #\Space (code-char 13) (code-char 10)) content)))
    (print (list code header content))))

;;; communicate ;;;

(defun docker-connect (host port &optional (test-p t))
  (let ((socket (socket-connect host port)))
    (if test-p
      (docker-ping socket test-p))
    socket))

(defmacro defdocker (name args &body body)
  `(defun ,name ,(cons 'socket args)
    (let ((stream (socket-stream socket)))
      ,@body
      (docker-response-read stream))))
