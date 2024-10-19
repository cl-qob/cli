;;; _prepare.el --- Prepare for command tasks
;;; Commentary: Prepare to setup Qob environment for sandboxing
;;; Code:

(require "asdf")

(defvar qob-dot-global (uiop:getenv "QOB_DOT_GLOBAL")
  "Return the global .qob directory.")

(defvar qob-dot-local (uiop:getenv "QOB_DOT_LOCAL")
  "Return the local .qob directory.")

(defvar qob-temp-filename (uiop:merge-pathnames* qob-dot-global "TMP")
  "Return the temp buffer filename.")

;;
;;; Utils

(defun qob--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (<= len 1) form-1 form-2)))

(defun qob-import (url)
  "Load and eval the script from a URL."
  (let ((bytes (dex:get url)))
    (with-open-file (out qob-temp-filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type 'unsigned-byte)
      (write-sequence bytes out))))

;;
;;; Package

(defun qob-install-quicklisp ()
  "Install Quicklisp."
  ;; TODO: ..
  (quicklisp-quickstart:install))

;;
;;; Verbose

(defun qob-princ (stream fmt &rest args)
  "Root print function with STREAM.

The argument STREAM is used to decide weather the stream should be standard
output or standard error.

The arguments FMT and ARGS are used to form the output message."
  (apply #'format (case stream
                    (`stdout *standard-output*)
                    (`stderr *error-output*)
                    (t t))
         fmt args))

(defun qob-print (msg &rest args)
  "Standard output print MSG and ARGS."
  (apply #'qob-princ 'stdout msg args))

(defun qob-println (msg &rest args)
  "Like function `qob-print' but with newline at the end."
  (apply #'qob-print msg args)
  (terpri))

(defun qob-msg (msg &rest args)
  "Standard error print MSG and ARGS."
  (apply #'qob-princ 'stderr msg args)
  (terpri))

(defun qob-info (msg &rest args)
  ""
  (qob-princ )
  )

;;
;;; Core

(defmacro qob-start (&rest body)
  "Execute BODY with workspace setup."
  `(progn
     (push (uiop:getcwd) asdf:*central-registry*)
     ,@body))

;; (defun qob-setup ()
;;   "Setup the system."
;;   (let ((files (asd-files t)))
;;     (mapc (lambda (file)
;;             (load-system file)
;;             (-info "Load ASD file ~A" file))
;;           files)))
;;
;; (defun qob-load-system (filename)
;;   "Load the system from ASD's FILENAME; and return the registered name."
;;   (let ((dir (uiop:pathname-parent-directory-pathname filename))
;;         (file (pathname-name filename)))
;;     (push dir asdf:*central-registry*)
;;     (asdf:load-system file)
;;     file))  ; registered name
;;
;; (defun qob-find-system (name)
;;   "Return a system of given NAME."
;;   (asdf/system-registry:registered-system name))
;;
;; (defun qob-asd-files (&optional with-test)
;;   "Return a list of ASD files.
;;
;; If optional argument WITH-TEST is non-nil; include test ASD files as well."
;;   (uiop:if-let ((files (directory "*.asd"))
;;                 (_ (not with-test))
;;                 (tests (asd-test-files)))
;;     (remove-if (lambda (filename) (el-lib:el-memq filename tests)) files)
;;     files))
;;
;; (defun qob-asd-test-files ()
;;   "Return a list of ASD test files."
;;   (directory "*-test*.asd"))
