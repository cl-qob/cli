;;; _prepare.el --- Prepare for command tasks
;;; Commentary: Prepare to setup Qob environment for sandboxing
;;; Code:

;;
;;; Includes

(require "asdf")

;;
;;; Verbose

(defun qob-princ (stream fmt &rest args)
  "Root print function with STREAM.

The argument STREAM is used to decide weather the stream should be standard
output or standard error.

The arguments FMT and ARGS are used to form the output message."
  (let ((stream (case stream
                  (stdout *standard-output*)
                  (stderr *error-output*)
                  (t t))))
    (apply #'format stream fmt args)))

(defun qob-print (msg &rest args)
  "Standard output print MSG and ARGS."
  (apply #'qob-princ 'stdout msg args))

(defun qob-println (msg &rest args)
  "Like function `qob-print' but with newline at the end."
  (apply #'qob-print (concatenate 'string msg "~%") args))

(defun qob-msg (msg &rest args)
  "Standard error print MSG and ARGS."
  (apply #'qob-princ 'stderr (concatenate 'string msg "~%") args))

(defun qob-trace (msg &rest args)
  "Send trace message; see function `qob--msg' for arguments MSG and ARGS."
  ;; TODO: ..
  (apply #'qob-msg msg args))

(defun qob-debug (msg &rest args)
  "Send debug message; see function `qob--msg' for arguments MSG and ARGS."
  ;; TODO: ..
  (apply #'qob-msg msg args))

(defun qob-info (msg &rest args)
  "Send info message; see function `qob--msg' for arguments MSG and ARGS."
  ;; TODO: ..
  (apply #'qob-msg msg args))

(defun qob-warn (msg &rest args)
  "Send warning message; see function `qob--msg' for arguments MSG and ARGS."
  ;; TODO: ..
  (apply #'qob-msg msg args))

(defun qob-error (msg &rest args)
  "Send error message; see function `qob--msg' for arguments MSG and ARGS."
  ;; TODO: ..
  (apply #'qob-msg msg args))

;;
;;; Environment

(defvar qob-lisp (uiop:getenv "QOB_LISP")
  "Return the current lisp implementation.")

(defvar qob-dot (uiop:getenv "QOB_DOT")
  "Return the current .qob directory.")

(defvar qob-temp-filename (uiop:getenv "QOB_TEMP_FILE")
  "Return the temp buffer filename.")

(defvar qob-lisp-root (uiop:getenv "QOB_LISP_ROOT")
  "Source `lisp' directory; should always end with slash.")

(defvar qob-user-init (uiop:getenv "QOB_USER_INIT")
  "Return the user init file.")

(defvar qob-quicklisp-installed-p (uiop:getenv "QOB_QUICKLISP_INSTALLED")
  "Return non-nil if Quicklisp is already installed.")

;;
;;; Elisp Layer

(defun el-memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

;;
;;; Utils

(defun qob--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (<= len 1) form-1 form-2)))

(defvar qob-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `qob-start' execution.")

(defun qob-script (script)
  "Return full SCRIPT filename."
  (concatenate 'string qob-lisp-root script ".lisp"))

(defun qob-call (script)
  "Call another qob SCRIPT."
  (let ((script-file (qob-script script)))
    (when (uiop:file-exists-p script-file)
      (load script-file)
      (qob-error "Script missing %s" script-file))))

(defun qob-load (script)
  "Load another eask SCRIPT; so we can reuse functions across all scripts."
  (let ((qob-loading-file-p t)) (qob-call script)))

;;
;;; Flags

(defun qob-global-p ()
  "Non-nil when in global space (`-g', `--global')."
  ;; TODO: ..
  nil)

(defun qob-local-p ()
  "Non-nil when in local space (default)."
  ;; TODO: ..
  t)

;;
;;; Package

(defconstant qob-source-mapping
  `((quicklisp . "https://www.quicklisp.org/")
    (ultralisp . "http://dist.ultralisp.org/"))
  "Mapping of source name and url.")

(defun qob-install-quicklisp ()
  "Install Quicklisp if not installed."
  (let* ((quicklisp-dir  (uiop:merge-pathnames* "quicklisp/" qob-dot))
         (quicklisp-init (uiop:merge-pathnames* "setup.lisp" quicklisp-dir)))
    (unless qob-quicklisp-installed-p
      (qob-quicklisp-install quicklisp-dir))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

;;
;;; Core

(defun qob-asd-test-files ()
  "Return a list of ASD test files."
  (directory "*-test*.asd"))

(defun qob-asd-files (&optional with-test)
  "Return a list of ASD files.

If optional argument WITH-TEST is non-nil; include test ASD files as well."
  (uiop:if-let ((files (directory "*.asd"))
                (_ (not with-test))
                (tests (qob-asd-test-files)))
    (remove-if (lambda (filename) (el-memq filename tests)) files)
    files))

(defun qob-load-system (filename)
  "Load the system from ASD's FILENAME; and return the registered name."
  (let ((dir (uiop:pathname-parent-directory-pathname filename))
        (file (pathname-name filename)))
    (push dir asdf:*central-registry*)
    (asdf:load-system file)
    file))  ; registered name

(defun qob-find-system (name)
  "Return a system of given NAME."
  (asdf/system-registry:registered-system name))

(defun qob-setup ()
  "Setup the system."
  (qob-install-quicklisp)
  ;; (let ((files (qob-asd-files t)))
  ;;   (mapc (lambda (file)
  ;;           (qob-load-system file)
  ;;           (qob-info "Load ASD file ~A" file))
  ;;         files))
  )

;;
;;; Externals

;;(qob-load "extern/alexandria")

;;; End of _prepare.lisp
