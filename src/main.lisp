(in-package :cl-maelstrom)

(defparameter *node-id* nil)

(defun handle-message-init (message)
  (setf *node-id* (serapeum:href-default 'missing-node-id message "body" "node_id"))
  (unless (eq *node-id* 'missing-node-id)
    (format *error-output* "Initialized node ~A~&" *node-id*))
  (reply message (serapeum:dict :type "init_ok"))) 

(defun handle-message-echo (message)
  (let ((body (href-default (dict) message "body")))
    (format *error-output* "Echoing ~A~&" body)
    (reply message (dict* body :type "echo_ok"))))

(serapeum.exporting:defun parse-input (line)
  (let ((parsed (yason:parse line)))
    (format *error-output* "Received ~A~&" parsed)
    (alexandria:eswitch ((href-default 'missing-type parsed "body" "type") :test #'equal)
                        ("init" (handle-message-init parsed))
                        ("echo" (handle-message-echo parsed)))))
