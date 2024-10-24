;;;; cmds/core/uninstall.lisp --- Uninstall dependencies

;;; Commentary
;;
;; The `uninstall' command definition.
;;

;;; Code

(defpackage qob-cli/uninstall
  (:use cl)
  (:export command))

(in-package :qob-cli/uninstall)

(defun options ()
  "Options for `uninstall' command."
  (list ))

(defun handler (cmd)
  "Handler for `uninstall' command."
  (qob-cli:call-script "core/uninstall" cmd))

(defun command ()
  "The `uninstall' command."
  (clingon:make-command
   :name "uninstall"
   :description "Uninstall packages"
   :usage "[names..]"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/uninstall.lisp
