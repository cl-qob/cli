;;;; src/logger.lisp --- Logger make easy

;;; Commentary
;;
;; Logger manage standard output, input, and error.
;;

;;; Code

(in-package :qob)

(defvar log-level 0)

(defun -print (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defun -trace (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defun -debug (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defun -info (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defun -warning (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defun -error (fmt &rest args)
  "TODO: .."
  (apply #'format t fmt args))

(defvar elapsed-time nil
  "Log with elapsed time.")

(defvar minimum-reported-time 0.1
  "Minimal load time that will be reported.")

(defmacro with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix (MSG-START) and suffix (MSG-END) messages."
  (declare (indent 0) (debug t))
  `(if elapsed-time
       (let ((now (current-time)))
         (ignore-errors (-write ,msg-start)) ,body
         (let ((elapsed (float-time (time-subtract (current-time) now))))
           (if (< elapsed minimum-reported-time)
               (ignore-errors (-msg ,msg-end))
               (ignore-errors (-write ,msg-end))
               (-msg (ansi-white (format " (%.3fs)" elapsed))))))
       (ignore-errors (-write ,msg-start)) ,body
       (ignore-errors (-msg ,msg-end))))
