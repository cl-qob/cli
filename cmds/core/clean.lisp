;;;; cmds/core/clean.lisp --- Clean up

;;; Commentary
;;
;; The `clean' command definition.
;;

;;; Code

(defpackage qob-cli/clean
  (:use cl)
  (:export command))

(in-package :qob-cli/clean)

(defun options ()
  "Options for `clean' command."
  (list ))

(defun handler (cmd)
  "Handler for `clean' command."
  (clingon:print-usage-and-exit cmd t))

(defun command ()
  "The `clean' command."
  (clingon:make-command
   :name "clean"
   :description "Delete various files produced during building"
   :usage "<type>"
   :options (options)
   :handler #'handler
   :sub-commands `(,(qob-cli/clean/dist:command)
                   ,(qob-cli/clean/workspace:command))))

;;; End of cmds/core/clean.lisp
