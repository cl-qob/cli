;;;; cmds/core/init.lisp --- Initialize project to use Qob

;;; Commentary
;;
;; The `init' command definition.
;;

;;; Code

(defpackage qob-cli/init
  (:use cl)
  (:export command))

(in-package :qob-cli/init)

(defun options ()
  "Options for `init' command."
  (list ))

(defun handler (cmd)
  "Handler for `init' command."
  (let ((qob-cli:force-global-p t))
    (qob-cli:call-script "core/init" cmd)))

(defun command ()
  "The `init' command."
  (clingon:make-command
   :name "init"
   :description "Initialize project to use Qob"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/init.lisp
