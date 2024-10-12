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
;;    --name, -n         path to the ASD file
;;    --output, -o       output directory
;;

;;; Code

(defpackage qob/build
  (:use cl)
  (:export command))

(in-package :qob/build)

(defun options ()
  "Options for the `build' command."
  (list
   (clingon:make-option
    :string
    :description "system name"
    :short-name #\n
    :long-name "name"
    :required t
    :key :name)
   (clingon:make-option
    :string
    :description "output directory"
    :short-name #\o
    :long-name "output"
    :required t
    :key :output)))

(defun handler (cmd)
  "Handler for the `build' command."
  (let* ((name   (clingon:getopt cmd :name))
         (output (clingon:getopt cmd :output)))
    ;;(format t "~A" (asdf/system-registry:registered-system name))
    ;;(format t "~A" output)
    ;; TODO: Change build path.
    (qob:setup)
    (asdf:operate :build-op name)
    ))

(defun command ()
  "Build command."
  (clingon:make-command
   :name "build"
   :description "Build the executable"
   :usage "-n <name> -o <path>"
   :options (options)
   :handler #'handler))
