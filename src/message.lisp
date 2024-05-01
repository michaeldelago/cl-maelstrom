(in-package :cl-maelstrom)

(defstruct message
  (src  "" :type string)
  (dest "" :type string)
  (body nil))

(defmethod yason:encode ((message message) &optional (stream *standard-output*))
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-element "src" (message-src message))
      (yason:encode-object-element "dest" (message-dest message))
      (yason:encode-object-element "body" (message-body message)))))

(defun message-init-ok ()
  (make-message :body '((:type . "init_ok"))))

(defun message-echo-ok (body)
  (let ((echo-body (copy-alist body)))
    (setf (alexandria:assoc-value echo-body :type) "echo_ok")
    (make-message :body echo-body)))

(defun message-topology-ok ()
  (make-message :body '((:type . "topology_ok"))))

(defun message-read-ok (messages)
  (make-message :body `((:type . "read_ok")
                        (:messages . ,messages))))

(defun message-broadcast (message)
  (make-message :body `((:type . "broadcast")
                        (:message . ,message))))

(defun message-broadcast-ok ()
  (make-message :body '((:type . "broadcast_ok"))))

(defun message-add-ok ()
  (make-message :body '((:type . "add_ok"))))
