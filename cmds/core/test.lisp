;;;; cmds/core/test.lisp --- Run system tests

;;; Commentary
;;
;; The `test' command definition.
;;

;;; Code

(defpackage qob-cli/test
  (:use cl)
  (:export command))

(in-package :qob-cli/test)

(defun options ()
  "Options for `test' command."
  (list ))

(defun handler (cmd)
  "Handler for `test' command."
  (qob-cli:call-script "core/test" cmd))

(defun command ()
  "The `test' command."
  (clingon:make-command
   :name "test"
   :description "Run system tests"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/test.lisp
