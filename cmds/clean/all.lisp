;;;; cmds/clean/all.lisp --- Do all cleaning tasks

;;; Commentary
;;
;; The `clean all' command definition.
;;

;;; Code

(defpackage qob-cli/clean/all
  (:use cl)
  (:export command))

(in-package :qob-cli/clean/all)

(defun options ()
  "Options for `clean all' command."
  (list ))

(defun handler (cmd)
  "Handler for `clean all' command."
  (qob-cli:call-script "clean/all" cmd))

(defun command ()
  "The `clean all' command."
  (clingon:make-command
   :name "all"
   :description "Do all cleaning tasks"
   :options (options)
   :handler #'handler))

;;; End of cmds/clean/all.lisp
