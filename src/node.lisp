(in-package :cl-maelstrom)

(defstruct node
  id
  other-node-ids
  neighbors
  messages)

(defun plog (log-message)
  (serapeum:synchronized (*error-output*)
    (format *error-output* "~A~&" log-message)))

(defmethod parse-input ((node node) line)
  (let ((parsed (yason:parse line)))
    (bt2:make-thread (serapeum:op 
                       (plog (format nil "Received ~A~&" parsed))
                       (alexandria:eswitch ((accesses parsed :body :type) :test #'equal)
                         ("init" (handle-init node parsed))
                         ("echo" (handle-echo node parsed))
                         ("topology" (handle-topology node parsed))
                         ("read" (handle-read node parsed))
                         ("broadcast" (handle-broadcast node parsed)))))))

(defmethod send ((node node) message)
  (setf (message-src message) (node-id node))
  (serapeum:synchronized (*standard-output*)
    (yason:encode message *standard-output*)
    (format t "~%")))

(defmethod reply ((node node) message response)
  (setf (message-dest response) (accesses message :src))
  (setf (accesses (message-body response) :in_reply_to) (accesses message :body :msg_id))
  (send node response))

(defun handle-init (node message)
  (with-slots (id other-node-ids)
    node
    (setf id (accesses message :body :node_id))
    (setf other-node-ids (accesses message :body :node_ids)))
  (plog (format nil "Initialized Node ~A~&" (node-id node)))
  (reply node message (message-init-ok)))

(defun handle-echo (node message)
  (let ((body (accesses message :body)))
    (plog (format nil "Echoing ~A~&" body))
    (reply node message (message-echo-ok body))))

(defun handle-topology (node message)
  (with-slots (id neighbors) node
    (setf neighbors (accesses message :body :topology (intern (string-upcase id) "KEYWORD")))
    (plog (format nil "My neighbors are ~A~&" neighbors)))
  (reply node message (message-topology-ok)))

(defun handle-read (node message)
  (serapeum:synchronized ((node-messages node))
    (reply node message (message-read-ok (node-messages node)))))

(defun handle-broadcast (node message)
  (let ((m (accesses message :body :message)))
    (serapeum:synchronized ((node-messages node))
      (unless (member m (node-messages node) :test #'equal)
        (push m (node-messages node))
        (dolist (neighbor (node-neighbors node))
          (unless (equal neighbor (accesses message :src))
            (let ((message-forward (message-broadcast m)))
              (setf (message-dest message-forward) neighbor)
              (send node message-forward)))))))
  (when (accesses message :body :msg_id)
    (reply node message (message-broadcast-ok))))
