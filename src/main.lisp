;;;; src/main.lisp --- Program entry

;;; Commentary
;;
;; Where the program start the execution.
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

(defun main ()
  "The main entry point of our CLI program."
  (let ((app (command)))
    (clingon:run app)))
