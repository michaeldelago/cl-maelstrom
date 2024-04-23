(in-package :cl-maelstrom)

(setq yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)

(defun send (dest msg-id body)
  (yason:encode (dict
                  :src (or *node-id* "missing")
                  :dest dest
                  :body (dict*
                          body
                          :msg_id msg-id))
                *standard-output*)
  (princ #\Newline))

(defun reply (request body)
  (let* ((msg-id (href-default -2 request "body" "msg_id"))
         (reply-id (1+ msg-id))
         (dest (href-default nil request "src")))
    (send dest reply-id (dict* body :in_reply_to msg-id))))
