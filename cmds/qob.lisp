;;;; cmds/qob.lisp --- The topmost level command

;;; Commentary
;;
;; The Qob (topmost level) command.
;;

;;; Code

(in-package :qob)

(defun options ()
  "Options for `qob' command."
  (list
   (clingon:make-option
    :integer
    :description "set verbosity from 0 to 5"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)))

(defun handler (cmd)
  "Handler for `qob' command."
  (clingon:print-usage-and-exit cmd t))

(defun command ()
  "A command to greet someone."
  (clingon:make-command
   :name "qob"
   :description "CLI for building, running, testing, and managing your Common Lisp dependencies"
   :version "0.1.0"
   :authors '("Jen-Chieh Shen <jcs090218@gmail.com>")
   :license "MIT"
   :options (options)
   :handler #'handler
   :sub-commands `(,(qob/build:command)
                   ,(qob/dists:command)
                   ,(qob/install:command)
                   ,(qob/list:command))))
