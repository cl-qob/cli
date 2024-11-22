;;;; cmds/clean/dist.lisp --- Clean up dist directory

;;; Commentary
;;
;; The `clean dist' command definition.
;;

;;; Code

(defpackage qob-cli/clean/dist
  (:use cl)
  (:export command))

(in-package :qob-cli/clean/dist)

(defun options ()
  "Options for `clean dist' command."
  (list ))

(defun handler (cmd)
  "Handler for `clean dist' command."
  (let ((qob-cli:inhibit-ql-download-p t))
    (qob-cli:call-script "clean/dist" cmd)))

(defun command ()
  "The `clean dist' command."
  (clingon:make-command
   :name "dist"
   :description "Delete dist subdirectory"
   :options (options)
   :handler #'handler))

;;; End of cmds/clean/dist.lisp
