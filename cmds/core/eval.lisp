;;;; cmds/core/eval.lisp --- Evaluate lisp form with a proper PATH

;;; Commentary
;;
;; The `eval' command definition.
;;

;;; Code

(defpackage qob-cli/eval
  (:use cl)
  (:export command))

(in-package :qob-cli/eval)

(defun options ()
  "Options for `eval' command."
  (list ))

(defun handler (cmd)
  "Handler for `eval' command."
  (qob-cli:call-script "core/eval" cmd))

(defun command ()
  "The `eval' command."
  (clingon:make-command
   :name "eval"
   :description "Evaluate lisp form with a proper PATH"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/eval.lisp
