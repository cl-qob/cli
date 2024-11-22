;;;; cmds/clean/workspace.lisp --- Clean up .qob directory

;;; Commentary
;;
;; The `clean workspace' command definition.
;;

;;; Code

(defpackage qob-cli/clean/workspace
  (:use cl)
  (:export command))

(in-package :qob-cli/clean/workspace)

(defun options ()
  "Options for `clean workspace' command."
  (list ))

(defun handler (cmd)
  "Handler for `clean workspace' command."
  (let ((qob-cli:inhibit-ql-download-p t))
    (qob-cli:call-script "clean/workspace" cmd)))

(defun command ()
  "The `clean workspace' command."
  (clingon:make-command
   :name "workspace"
   :description "Clean up .qob directory"
   :aliases `(".qob")
   :options (options)
   :handler #'handler))

;;; End of cmds/clean/workspace.lisp
