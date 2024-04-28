(in-package :cl-maelstrom)

(defun plog (message)
  (serapeum:synchronized (*error-output*)
   (format *error-output* "~A" message)))

(defun send (dest msg-id body)
  (serapeum:synchronized (*standard-output*)
    (yason:encode-alist (pairlis '(:src :dest :body)
                           (list *node-id* dest (progn (setf (accesses body :msg_id) msg-id) body)))
                  *standard-output*))
  (princ #\Newline))

(defun reply (request body)
  (let* ((msg-id (accesses request :body :msg_id))
         (reply-id (1+ msg-id))
         (dest (accesses request :src)))
    (send dest reply-id (progn (setf (accesses body :in_reply_to) msg-id) body))))
