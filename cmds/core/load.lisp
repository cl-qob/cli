;;;; cmds/core/load.lisp --- Load lisp files

;;; Commentary
;;
;; The `load' command definition.
;;

;;; Code

(defpackage qob-cli/load
  (:use cl)
  (:export command))

(in-package :qob-cli/load)

(defun options ()
  "Options for `load' command."
  (list ))

(defun handler (cmd)
  "Handler for `load' command."
  (qob-cli:call-script "core/load" cmd))

(defun command ()
  "The `load' command."
  (clingon:make-command
   :name "load"
   :description "Load lisp files"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/load.lisp
