;;;; cmds/core/install-deps.lisp --- Build executable

;;; Commentary
;;
;; The `install-deps' command definition.
;;

;;; Code

(defpackage qob-cli/install-deps
  (:use cl)
  (:export command))

(in-package :qob-cli/install-deps)

(defun options ()
  "Options for `install-deps' command."
  (list
   (clingon:make-option
    :flag
    :description "install include development dependencies"
    :long-name "dev"
    :key :dev)))

(defun handler (cmd)
  "Handler for `install-deps' command."
  (qob-cli:call-script "core/install-deps" cmd))

(defun command ()
  "The `install-deps' command."
  (clingon:make-command
   :name "install-deps"
   :description "Automatically install system dependencies"
   :usage "[names..]"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/install-deps.lisp
