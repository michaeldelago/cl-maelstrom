(in-package :cl-maelstrom)

(serapeum.exporting:defun scheduled-task (period-seconds fun)
  "Create a thread with a sleep period and a function to execute after that interval. Loops indefinitely"
  (bt2:make-thread
    (op (loop 
          do (progn (funcall fun)
                    (sleep period-seconds))))))
