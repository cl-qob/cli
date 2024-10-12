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
