(in-package :cl-maelstrom)

(defstruct node
  id
  other-node-ids
  neighbors
  messages
  message-set
  callbacks
  scheduled-tasks
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
                               (synchronized (callbacks)
                                 ;; Remove callback from node callbacks after it's executed
                                 (setf callbacks (remove in-reply-to callbacks :key 'car))))))
                         (alexandria:eswitch ((accesses parsed :body :type) :test #'equal)
                           ("init" (handle-init node parsed))
                           ("echo" (handle-echo node parsed))
                           ("topology" (handle-topology node parsed))
                           ("read" (handle-read node parsed))
                           ("broadcast" (handle-broadcast node parsed))
                           ("add" (handle-add node parsed))
                           ("replicate" (handle-replicate node parsed))))))))

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
  (if (node-id node)
      (plog (format nil "Node ~A already initialized~&" (node-id node)))
      (progn 
        (with-slots (id other-node-ids)
          node
          (setf id (accesses message :body :node_id))
          (setf other-node-ids (accesses message :body :node_ids)))
        (flet ((replicate ()
                 (let ((message-set (synchronized ((node-message-set node)) 
                                      (copy-list (node-message-set node)))))
                   (when (node-other-node-ids node)
                     (plog (format nil "Replicating current set ~A~&" message-set))
                     (dolist (recipient (node-other-node-ids node))
                       (unless (equal (node-id node) recipient)
                         (send node (message-with-dest recipient 
                                               (message-replicate message-set)))))))))
          (synchronized ((node-scheduled-tasks node))
            (push (scheduled-task 5 #'replicate) (node-scheduled-tasks node))))
        (plog (format nil "Initialized Node ~A~&" (node-id node)))
        (reply node message (message-init-ok)))))

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
  (with-slots (messages message-set) node
    (synchronized (messages)
      (synchronized (message-set)
        (reply node message (message-read-ok messages message-set))))))

(defun handle-broadcast (node message)
  (reply node message (message-broadcast-ok))
  (let ((m (accesses message :body :message))
        new-message)
    (synchronized ((node-messages node))
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
  (with-slots (message-set) node
    (synchronized (message-set)
    (setf message-set (adjoin (accesses message :body :element) message-set))))
  (reply node message (message-add-ok)))

(defun handle-replicate (node message)
  (with-slots (message-set) node
    (synchronized (message-set)
      (setf message-set (union message-set (accesses message :body :value))))))
  

(defun rand-subset (seq &optional (len 10))
  (remove-duplicates 
    (mapcar (op (elt seq _)) (serapeum:with-collector (collect) 
                                        (dotimes (n (min len (length seq)))
                                          (collect (random (length seq))))))))

(defun rpc (node message callback)
  (synchronized ((node-callbacks node))
    (let ((msg-id (bt2:atomic-integer-incf (node-next-msg-id node))))
      (push (cons msg-id callback) (node-callbacks node))
      (setf (accesses (message-body message) :msg_id) msg-id)))
  (plog (format nil "Sending rpc message ~A~&" message))
  (send node message))
