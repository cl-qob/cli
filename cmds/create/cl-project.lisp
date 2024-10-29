;;;; cmds/create/cl-project.lisp --- Clean up

;;; Commentary
;;
;; The `cl-project' command definition.
;;

;;; Code

(defpackage qob-cli/create/cl-project
  (:use cl)
  (:export command))

(in-package :qob-cli/create/cl-project)

(defun options ()
  "Options for `create cl-project' command."
  (list ))

(defun handler (cmd)
  "Handler for `create cl-project' command."
  (let ((qob-cli:force-global-p t))
    (qob-cli:call-script "create/cl-project" cmd)))

(defun command ()
  "The `create cl-project' command."
  (clingon:make-command
   :name "cl-project"
   :description "Create a new project using `cl-project'"
   :usage "<name>"
   :options (options)
   :handler #'handler))

;;; End of cmds/create/cl-project.lisp
