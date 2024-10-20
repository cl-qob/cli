;;;; cmds/core/install.lisp --- Build executable

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
  (declare (ignore cmd))
  (qob-cli:call-script "core/install" cmd))

(defun command ()
  "The `install' command."
  (clingon:make-command
   :name "install"
   :description "Install packages"
   :usage "[names..]"
   :options (options)
   :handler #'handler))
