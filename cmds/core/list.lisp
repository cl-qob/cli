;;;; cmds/core/list.lisp --- Build executable

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
  (declare (ignore cmd))
  (qob-cli:call-script "core/list" cmd))

(defun command ()
  "The `list' command."
  (clingon:make-command
   :name "list"
   :description "List the registered system"
   :options (options)
   :handler #'handler))
