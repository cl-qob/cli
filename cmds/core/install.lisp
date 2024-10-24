;;;; cmds/core/install.lisp --- Install systems

;;; Commentary
;;
;; The `install' command definition.
;;

;;; Code

(defpackage qob-cli/install
  (:use cl)
  (:export command))

(in-package :qob-cli/install)

(defun options ()
  "Options for `install' command."
  (list ))

(defun handler (cmd)
  "Handler for `install' command."
  (qob-cli:call-script "core/install" cmd))

(defun command ()
  "The `install' command."
  (clingon:make-command
   :name "install"
   :description "Install systems"
   :usage "[names..]"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/install.lisp
