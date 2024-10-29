;;;; cmds/core/create.lisp --- Clean up

;;; Commentary
;;
;; The `create' command definition.
;;

;;; Code

(defpackage qob-cli/create
  (:use cl)
  (:export command))

(in-package :qob-cli/create)

(defun options ()
  "Options for `create' command."
  (list ))

(defun handler (cmd)
  "Handler for `create' command."
  (clingon:print-usage-and-exit cmd t))

(defun command ()
  "The `create' command."
  (clingon:make-command
   :name "create"
   :description "Create a new Common Lisp project"
   :usage "<type>"
   :options (options)
   :handler #'handler
   :sub-commands `(,(qob-cli/create/cl-project:command))))

;;; End of cmds/core/create.lisp
