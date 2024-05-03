(in-package :cl-maelstrom)

(defparameter *printer* nil)

(defstruct printer
  (rx-stdout (make-instance 'calispel:channel))
  (rx-stderr (make-instance 'calispel:channel))
  stdout
  stderr)

(serapeum.exporting:defun start-printer (&optional (stdout *standard-output*) (stderr *error-output*))
  (let ((printer (make-printer :stdout stdout :stderr stderr)))
    (bt2:make-thread
      (lambda ()
        (loop
          do (calispel:pri-alt
               ((calispel:? (printer-rx-stdout printer) message)
                (format (printer-stdout printer) message))
               ((calispel:? (printer-rx-stderr printer) emessage)
                (format (printer-stderr printer) emessage))))))
    printer))

(serapeum.exporting:defun print-stdout (message &key (printer *printer*))
  (calispel:! (printer-rx-stdout printer) message))

(serapeum.exporting:defun print-stderr (message &key (printer *printer*))
  (calispel:! (printer-rx-stderr printer) message))
