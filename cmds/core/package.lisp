;;;; cmds/core/package.lisp --- Build a system artifact

;;; Commentary
;;
;; The `package' command definition.
;;

;;; Code

(defpackage qob-cli/package
  (:use cl)
  (:export command))

(in-package :qob-cli/package)

(defun options ()
  "Options for `package' command."
  (list ))

(defun handler (cmd)
  "Handler for `package' command."
  (qob-cli:call-script "core/package" cmd))

(defun command ()
  "The `package' command."
  (clingon:make-command
   :name "package"
   :description "Build a system artifact"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/package.lisp
