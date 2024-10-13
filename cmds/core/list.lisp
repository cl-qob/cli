;;;; cmds/core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(defpackage qob/list
  (:use cl)
  (:export command))

(in-package :qob/list)

(defun options ()
  "Options for `list' command."
  (list ))

(defun handler (cmd)
  "Handler for `list' command."
  (declare (ignore cmd))
  (qob:setup)
  (format t "~A" (asdf/system-registry:registered-systems)))

(defun command ()
  "List command."
  (clingon:make-command
   :name "list"
   :description "List the registered system"
   :options (options)
   :handler #'handler))
