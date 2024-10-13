;;;; cmds/core/build.lisp --- Build executable

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
  "Options for `build' command."
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
    :key :output)))

(defun handler (cmd)
  "Handler for `build' command."
  (let* ((name   (clingon:getopt cmd :name))
         (output (clingon:getopt cmd :output)))
    ;; Delete if exists to prevent errors.
    (when (uiop:file-exists-p output)
      (delete-file output))
    ;; TODO: Change build path.
    (qob:setup)
    (asdf:operate :build-op name)))

(defun command ()
  "Build command."
  (clingon:make-command
   :name "build"
   :description "Build the executable"
   :usage "-n <name> -o <path>"
   :options (options)
   :handler #'handler))
