;;;; src/main.lisp --- Program entry

;;; Commentary
;;
;; Where the program start the execution.
;;

;;; Code

(in-package :qob)

(defun main ()
  "The main entry point of our CLI program."
  (let ((app (command)))
    (clingon:run app)))
