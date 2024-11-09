;;;; cmds/qob.lisp --- The topmost level command

;;; Commentary
;;
;; The Qob (topmost level) command.
;;

;;; Code

(in-package :qob-cli)

(defun options ()
  "Options for `qob' command."
  (list
   (clingon:make-option
    :flag
    :description "change default workspace to ~/.qob/"
    :short-name #\g
    :long-name "global"
    :persistent t
    :key :global)
   (clingon:make-option
    :flag
    :description "enable all flag"
    :short-name #\a
    :long-name "all"
    :persistent t
    :key :all)
   (clingon:make-option
    :flag
    :description "enable/disable color output"
    :long-name "no-color"
    :persistent t
    :key :no-color)
   (clingon:make-option
    :integer
    :description "set verbosity from 0 to 5"
    :short-name #\v
    :long-name "verbose"
    :initial-value 3
    :persistent t
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
   :sub-commands `(,(qob-cli/build:command)
                   ,(qob-cli/clean:command)
                   ,(qob-cli/create:command)
                   ,(qob-cli/dists:command)
                   ,(qob-cli/info:command)
                   ,(qob-cli/install:command)
                   ,(qob-cli/install-deps:command)
                   ,(qob-cli/install-dists:command)
                   ,(qob-cli/list:command)
                   ,(qob-cli/locate:command)
                   ,(qob-cli/status:command)
                   ,(qob-cli/uninstall:command))))

;;; End of cmds/qob.lisp
