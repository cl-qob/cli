;;;; cmds/core/install-dists.lisp --- Build executable

;;; Commentary
;;
;; The `install-dists' command definition.
;;

;;; Code

(defpackage qob-cli/install-dists
  (:use cl)
  (:export command))

(in-package :qob-cli/install-dists)

(defun options ()
  "Options for `install-dists' command."
  (list ))

(defun handler (cmd)
  "Handler for `install-dists' command."
  (qob-cli:call-script "core/install-dists" cmd))

(defun command ()
  "The `install-dists' command."
  (clingon:make-command
   :name "install-dists"
   :description "Install dists"
   :usage "[names..]"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/install-dists.lisp
