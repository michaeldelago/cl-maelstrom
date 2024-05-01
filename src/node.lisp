(in-package :cl-maelstrom)

(defstruct node
  id
  other-node-ids
  neighbors
  messages
  callbacks
  (next-msg-id (bt2:make-atomic-integer)))

(defun plog (log-message)
  (print-stderr (format nil "~A~&" log-message)))

(defmethod parse-input ((node node) line)
  (let ((parsed (yason:parse line)))
    (bt2:make-thread (lambda ()
                       (plog (format nil "Received ~A~&" parsed))
                       (alexandria:if-let ((in-reply-to (accesses parsed :body :in_reply_to)))
                         (with-slots (callbacks) node
                           (let ((handler (accesses callbacks in-reply-to)))
                             (prog1
                               (funcall handler node parsed)
                               (serapeum:synchronized (callbacks)
                                 ;; Remove callback from node callbacks after it's executed
                                 (setf callbacks (remove in-reply-to callbacks :key 'car))))))
                         (alexandria:eswitch ((accesses parsed :body :type) :test #'equal)
                           ("init" (handle-init node parsed))
                           ("echo" (handle-echo node parsed))
                           ("topology" (handle-topology node parsed))
                           ("read" (handle-read node parsed))
                           ("broadcast" (handle-broadcast node parsed))
                           ("add" (handle-add node parsed))))))))

(defmethod send ((node node) message)
  (setf (message-src message) (node-id node))
    (print-stdout (format nil "~A~&" (yason:encode message))))

(defmethod reply ((node node) message response)
  (with-slots (dest body)
    response
    (setf dest (accesses message :src))
    (setf (accesses body :in_reply_to) (accesses message :body :msg_id))
    (setf (accesses body :msg_id) (bt2:atomic-integer-incf (node-next-msg-id node)))
    (send node response)))

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
  (reply node message (message-broadcast-ok))
  (let ((m (accesses message :body :message))
        new-message)
    (serapeum:synchronized ((node-messages node))
      (unless (find m (node-messages node) :test #'equal)
        (push m (node-messages node))
        (setf new-message t)))
    (when new-message
      (let ((unacked (remove-if (op (equal _ (accesses message :src))) (node-neighbors node))))
        (loop while (not (alexandria:emptyp unacked))
              do (dolist (neighbor unacked)
                   (let ((message-forward (message-broadcast m)))
                     (setf (message-dest message-forward) neighbor)
                     (rpc node message-forward (lambda (node response)
                                                 (plog (format nil "Got response ~A~&" response))
                                                 (when (equal (accesses response :body :type) "broadcast_ok")
                                                   (setf unacked (remove neighbor unacked :test #'equal))))))
                   (sleep 1)))))))

(defun handle-add (node message)
  (serapeum:synchronized ((node-messages node))
    (setf (node-messages node) (adjoin (accesses message :body :message) (node-messages node))))
  (reply node message (message-add-ok)))

(defun handle-replicate (node message)
  (with-slots (messages) node
    (serapeum:synchronized (messages)
      (setf messages (union messages (accesses message :body :value))))))
  

(defun rand-subset (seq &optional (len 10))
  (remove-duplicates 
    (mapcar (serapeum:op (elt seq _)) (serapeum:with-collector (collect) 
                                        (dotimes (n (min len (length seq)))
                                          (collect (random (length seq))))))))

(defun rpc (node message callback)
  (serapeum:synchronized ((node-callbacks node))
    (let ((msg-id (bt2:atomic-integer-incf (node-next-msg-id node))))
      (push (cons msg-id callback) (node-callbacks node))
      (setf (accesses (message-body message) :msg_id) msg-id)))
  (plog (format nil "Sending rpc message ~A~&" message))
  (send node message))
