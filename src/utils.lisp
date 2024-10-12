;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defun setup ()
  "Setup system."
  (push '*default-pathname-defaults* asdf:*central-registry*))

(defun load-system (filename)
  "Load the system from ASD's FILENAME."
  (let ((dir (uiop:pathname-parent-directory-pathname filename))
        (file (pathname-name filename)))
    (push dir asdf:*central-registry*)
    (asdf:load-system file)
    file))

(defun find-system (name)
  "Return a system of given NAME."
  (asdf/system-registry:registered-system name))

(defun asd-files ()
  "Return a list of asd files."
  (directory "*.asd"))

(defun asd-test-files ()
  "Return a list of asd test files."
  (directory "*-test*.asd"))
