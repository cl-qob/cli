;;;; cmds/qob.lisp --- The topmost level command

;;; Commentary
;;
;; The Qob (topmost level) command.
;;

;;; Code

(in-package :qob)

(defun handler (cmd)
  "Handler for the `qob' command."
  (clingon:print-usage-and-exit cmd t))

(defun command ()
  "A command to greet someone."
  (clingon:make-command
   :name "qob"
   :description "CLI for building, running, testing, and managing your Common Lisp dependencies"
   :version "0.1.0"
   :authors '("Jen-Chieh Shen <jcs090218@gmail.com>")
   :license "MIT"
   :handler #'handler
   :sub-commands `(,(qob/build:command)
                   ,(qob/list:command))))
