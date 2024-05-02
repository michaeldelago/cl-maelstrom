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

(defun message-read-ok (messages value)
  (let ((body '((:type . "read_ok"))))
    (when messages 
      (push (cons :messages messages) body))
    (push (cons :value value) body)
    (make-message :body body)))

(defun message-broadcast (message)
  (make-message :body `((:type . "broadcast")
                        (:message . ,message))))

(defun message-broadcast-ok ()
  (make-message :body '((:type . "broadcast_ok"))))

(defun message-add-ok ()
  (make-message :body '((:type . "add_ok"))))

(defun message-replicate (set)
  (make-message :body `((:type . "replicate")
                        (:value . ,set))))

(defun message-with-dest (dest message)
  (setf (message-dest message) dest)
  message)
