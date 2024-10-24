;;;; cmds/core/info.lisp --- Print the system info

;;; Commentary
;;
;; The `info' command definition.
;;

;;; Code

(defpackage qob-cli/info
  (:use cl)
  (:export command))

(in-package :qob-cli/info)

(defun options ()
  "Options for `info' command."
  (list ))

(defun handler (cmd)
  "Handler for `info' command."
  (qob-cli:call-script "core/info" cmd))

(defun command ()
  "The `info' command."
  (clingon:make-command
   :name "info"
   :description "Display information about the current system(s)"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/info.lisp
