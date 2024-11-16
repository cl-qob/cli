;;;; cmds/core/files.lisp --- Print all system files

;;; Commentary
;;
;; The `files' command definition.
;;

;;; Code

(defpackage qob-cli/files
  (:use cl)
  (:export command))

(in-package :qob-cli/files)

(defun options ()
  "Options for `files' command."
  (list ))

(defun handler (cmd)
  "Handler for `files' command."
  (qob-cli:call-script "core/files" cmd))

(defun command ()
  "The `files' command."
  (clingon:make-command
   :name "files"
   :description "Print all system files"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/files.lisp
