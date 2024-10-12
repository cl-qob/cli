;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(defpackage qob/utils
  (:use cl)
  (:export get-asd-files))

(in-package :qob/utils)

(defun asd-files ()
  "Return a list of asd files."
  (directory "*.asd"))

(defun asd-test-files ()
  "Return a list of asd test files."
  (directory "*-test*.asd"))
