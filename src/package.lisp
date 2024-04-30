(defpackage :cl-maelstrom
  (:use :cl)
  (:import-from :access #:accesses)
  (:export #:make-node
           #:parse-input))

(setf yason:*parse-object-as* :alist)
(setf yason:*parse-object-key-fn* (serapeum:op (intern (string-upcase _) "KEYWORD")))
(setf yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase)
(setf yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)
(setf yason:*list-encoder* #'yason:encode-alist)
