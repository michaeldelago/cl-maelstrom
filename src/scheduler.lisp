(in-package :cl-maelstrom)

(defun scheduled-task (period-seconds fun)
  (bt2:make-thread
    (op (loop 
          do (progn (funcall fun)
                    (sleep period-seconds))))))
