;;;; cmds/core/status.lisp --- Print the workspace status

;;; Commentary
;;
;; The `status' command definition.
;;

;;; Code

(defpackage qob-cli/status
  (:use cl)
  (:export command))

(in-package :qob-cli/status)

(defun options ()
  "Options for `status' command."
  (list ))

(defun handler (cmd)
  "Handler for `status' command."
  (qob-cli:call-script "core/status" cmd))

(defun command ()
  "The `status' command."
  (clingon:make-command
   :name "status"
   :description "Display the state of the workspace"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/status.lisp
