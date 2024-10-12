#| src/main.lisp

Program entry

Where the program start the execution.
|#

(defpackage qob
  (:nicknames qlot/main)
  (:use cl)
  (:export main))

(in-package :qob)

(ql:quickload "clingon")

(defun greet/options ()
  "Returns the options for the `greet' command"
  (list
   (clingon:make-option
    :string
    :description "Person to greet"
    :short-name #\u
    :long-name "user"
    :initial-value "stranger"
    :env-vars '("USER")
    :key :user)))

(defun greet/handler (cmd)
  "Handler for the `greet' command"
  (let ((who (clingon:getopt cmd :user)))
    (format t "Hello, ~A!~%" who)))

(defun greet/command ()
  "A command to greet someone."
  (clingon:make-command
   :name "greet"
   :description "greets people"
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org")
   :license "BSD 2-Clause"
   :options (greet/options)
   :handler #'greet/handler))

(defun main ()
  "The main entrypoint of our CLI program"
  (let ((app (greet/command)))
    (clingon:run app)))
