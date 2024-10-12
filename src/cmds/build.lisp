;;;; src/cmds/build.lisp --- Build executable

;;; Commentary
;;
;; Command use to build the executable
;;
;;   $ qob build
;;
;;
;;  Optional arguments:
;;
;;    --output, -o       output directory
;;

;;; Code

(defpackage qob/build
  (:use cl)
  (:export command))

(in-package :qob/build)

(defun options ()
  "Returns the options for the `build' command."
  (list
   (clingon:make-option
    :string
    :description "output directory"
    :short-name #\o
    :long-name "output"
    :initial-value "./"
    ;;:required t
    :key :output)))

(defun handler (cmd)
  "Handler for the `build' command."
  (let ((output (clingon:getopt cmd :output)))
    (format t "~A" output)
    ))

(defun command ()
  "Build the executable."
  (clingon:make-command
   :name "build"
   :description "Build the executable"
   :options (options)
   :handler #'handler))
