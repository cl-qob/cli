;;;; cmds/core/install.lisp --- Build executable

;;; Commentary
;;
;; The `install' command definition.
;;

;;; Code

(defpackage qob/install
  (:use cl)
  (:export command))

(in-package :qob/install)

(defun options ()
  "Options for `install' command."
  (list ))

(defun handler (cmd)
  "Handler for `list' command."
  (declare (ignore cmd))
  (qob:call-lisp "core/install"))

(defun command ()
  "List command."
  (clingon:make-command
   :name "install"
   :description "Install packages"
   :usage "[names..]"
   :options (options)
   :handler #'handler))
