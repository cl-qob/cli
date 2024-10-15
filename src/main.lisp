;;;; src/main.lisp --- Program entry

;;; Commentary
;;
;; Where the program start the execution.
;;

;;; Code

(in-package :qob)

(defvar *dot-root* ".qob/"
  "The .qob directory.")

(defun make-dot-folder ()
  "Create the dot folder."
  (ensure-directories-exist *dot-root*))

(defun main ()
  "The main entry point of our CLI program."
  (make-dot-folder)
  (let ((app (command)))
    (clingon:run app)))
