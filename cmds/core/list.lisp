;;;; cmds/core/list.lisp --- List the registered systems

;;; Commentary
;;
;; The `list' command definition.
;;

;;; Code

(defpackage qob-cli/list
  (:use cl)
  (:export command))

(in-package :qob-cli/list)

(defun options ()
  "Options for `list' command."
  (list ))

(defun handler (cmd)
  "Handler for `list' command."
  (qob-cli:call-script "core/list" cmd))

(defun command ()
  "The `list' command."
  (clingon:make-command
   :name "list"
   :description "List the registered systems"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/list.lisp
