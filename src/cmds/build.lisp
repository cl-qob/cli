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
    :description "path to the ASD file"
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
         (output (clingon:getopt cmd :output))
         (name   (qob:load-system name)))
    (format t "~A" (asdf/system-registry:registered-system name))
    (format t "~A" output)
    ;;(setq asdf/system:build-pathname output)
    ;;(asdf:operate :build-op name)
    ))

(defun command ()
  "Build the executable."
  (clingon:make-command
   :name "build"
   :description "Build the executable"
   :usage "-o /bin/name.exe"
   :options (options)
   :handler #'handler))
