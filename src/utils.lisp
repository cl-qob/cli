;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defun memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun setup ()
  "Setup the system."
  (let ((files (asd-files t)))
    (mapc (lambda (file)
            (load-system file)
            (-info "Load ASD file ~A" file))
          files)))

(defun load-system (filename)
  "Load the system from ASD's FILENAME; and return the registered name."
  (let ((dir (uiop:pathname-parent-directory-pathname filename))
        (file (pathname-name filename)))
    (push dir asdf:*central-registry*)
    (asdf:load-system file)
    file))  ; registered name

(defun find-system (name)
  "Return a system of given NAME."
  (asdf/system-registry:registered-system name))

(defun asd-files (&optional with-test)
  "Return a list of ASD files.

If optional argument WITH-TEST is non-nil; include test ASD files as well."
  (uiop:if-let ((files (directory "*.asd"))
                (_ (not with-test))
                (tests (asd-test-files)))
    (remove-if (lambda (filename) (memq filename tests)) files)
    files))

(defun asd-test-files ()
  "Return a list of ASD test files."
  (directory "*-test*.asd"))
