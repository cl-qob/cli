;;;; cmds/core/locate.lisp --- Print the Qob installed location

;;; Commentary
;;
;; The `locate' command definition.
;;

;;; Code

(defpackage qob-cli/locate
  (:use cl)
  (:export command))

(in-package :qob-cli/locate)

(defun options ()
  "Options for `locate' command."
  (list ))

(defun handler (cmd)
  "Handler for `locate' command."
  (format t "✓ Qob located in '~A'" (qob-cli:exec-dir)))

(defun command ()
  "The `locate' command."
  (clingon:make-command
   :name "locate"
   :description "Print out Qob installed location"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/locate.lisp
