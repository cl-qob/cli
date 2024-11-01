;;;; cmds/core/build.lisp --- Build executable

;;; Commentary
;;
;; The `build' command definition.
;;

;;; Code

(defpackage qob-cli/build
  (:use cl)
  (:export command))

(in-package :qob-cli/build)

(defun options ()
  "Options for `build' command."
  (list
   (clingon:make-option
    :string
    :description "system name"
    :short-name #\n
    :long-name "name"
    :key :name)
   (clingon:make-option
    :string
    :description "output directory"
    :short-name #\o
    :long-name "output"
    :key :output)
   (clingon:make-option
    :flag
    :description "compress output for smaller file size"
    :long-name "compression"
    :initial-value nil
    :key :compression)))

(defun handler (cmd)
  "Handler for `build' command."
  (qob-cli:call-script "core/build" cmd))

(defun command ()
  "The `build' command."
  (clingon:make-command
   :name "build"
   :description "Build the executable"
   :usage "[names..]"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/build.lisp
