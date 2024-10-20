;;;; src/main.lisp --- Program entry

;;; Commentary
;;
;; Where the program start the execution.
;;

;;; Code

(in-package :qob-cli)

(defun make-dot-folder ()
  "Create the dot folder."
  (ensure-directories-exist (dot-local)))

(defun main ()
  "The main entry point of our CLI program."
  (make-dot-folder)
  (let ((app (command)))
    (clingon:run app)))
