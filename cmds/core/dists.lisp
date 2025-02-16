;;;; cmds/core/dists.lisp --- Print all dists info

;;; Commentary
;;
;; The `dists' command definition.
;;

;;; Code

(defpackage qob-cli/dists
  (:use cl)
  (:export command))

(in-package :qob-cli/dists)

(defun options ()
  "Options for `dists' command."
  (list ))

(defun handler (cmd)
  "Handler for `dists' command."
  (qob-cli:call-script "core/dists" cmd))

(defun command ()
  "The `dists' command."
  (clingon:make-command
   :name "dists"
   :description "List out all installed dists"
   :options (options)
   :handler #'handler))

;;; End of cmds/core/dist.lisp
