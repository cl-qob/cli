;;; lisp/_prepare.lisp --- Prepare for command tasks
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
  (apply #'qob-msg msg args))

(defun qob-debug (msg &rest args)
  "Send debug message; see function `qob--msg' for arguments MSG and ARGS."
  (apply #'qob-msg msg args))

(defun qob-info (msg &rest args)
  "Send info message; see function `qob--msg' for arguments MSG and ARGS."
  (qob-msg (qob-ansi-cyan (format nil msg args))))

(defun qob-warn (msg &rest args)
  "Send warning message; see function `qob--msg' for arguments MSG and ARGS."
  (qob-msg (qob-ansi-yellow (format nil msg args))))

(defun qob-error (msg &rest args)
  "Send error message; see function `qob--msg' for arguments MSG and ARGS."
  (qob-msg (qob-ansi-red (format nil msg args))))

;;
;;; Environment

(defun qob-parse-args (env-name)
  "Parse arguments.

Argument ENV-NAME is used to get the argument string."
  (let* ((args (uiop:getenv env-name))
         (args (concatenate 'string "'" args)))
    (eval (read-from-string args))))

(defvar qob-args (qob-parse-args "QOB_ARGS")
  "Positionl arguments (no options).")

(defvar qob-opts (qob-parse-args "QOB_OPTS")
  "Options (no positional arguments).")

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

(defun qob-dot-home ()
  "Return the directory path to `.qob/type/version'.

For example, `.qob/sbcl/2.4.9/'."
  (uiop:merge-pathnames* (concatenate 'string
                                      (lisp-implementation-type) "/"
                                      (lisp-implementation-version) "/")
                         qob-dot))

;;
;;; Utils

(defun qob-2str (object)
  "Convert to string."
  (funcall #'qob-el-2str object))

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
  "Load another qob SCRIPT; so we can reuse functions across all scripts."
  (let ((qob-loading-file-p t)) (qob-call script)))

;;
;;; Flags

(defun qob--flag (flag)
  "Return non-nil if FLAG exists."
  (member flag qob-opts :test (lambda (a b)
                                ;; `string-equal' is not case-sensitive
                                (string-equal (qob-2str a)
                                              (qob-2str b)))))

(defun qob--flag-value (flag)
  "Return value for FLAG."
  (nth 1 (qob--flag flag)))

;;; Boolean
(defun qob-global-p ()
  "Non-nil when in global space (`-g', `--global')."
  (qob--flag "--global"))

(defun qob-local-p ()
  "Non-nil when in local space (default)."
  (not (qob-global-p)))

(defun qob-all-p ()
  "Non-nil when flag is on (`-a', `--all')."
  (qob--flag "--all"))

(defun qob-no-color-p ()
  "Non-nil when flag is on (`--no-color')."
  (qob--flag "--no-color"))

;;; Number (with arguments)
(defun qob-verbose ()
  "Non-nil when flag has value (`-v', `--verbose')."
  (qob--flag-value "--verbose"))

;;
;;; Package

(defconstant qob-source-mapping
  `((quicklisp . "https://www.quicklisp.org/")
    (ultralisp . "http://dist.ultralisp.org/"))
  "Mapping of source name and url.")

(defun qob-ql-installed-dir ()
  "Return the QuickLisp installed directory base on scope."
  (uiop:merge-pathnames* "quicklisp/" (if (qob-global-p)
                                          (user-homedir-pathname)
                                          (qob-dot-home))))

(defun qob-init-ql ()
  "Initialize QuickLisp."
  (let* ((ql-dir (qob-ql-installed-dir))
         (ql-init (uiop:merge-pathnames* "setup.lisp" ql-dir)))
    (unless qob-quicklisp-installed-p
      (qob-quicklisp-install ql-dir))
    (when (probe-file ql-init)
      (load ql-init))))

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
    (remove-if (lambda (filename) (qob-el-memq filename tests)) files)
    files))

(defun qob-load-system (filename)
  "Load the system from ASD's FILENAME; and return the registered name."
  (let ((dir (qob-el-file-name-directory filename))
        (file (pathname-name filename)))
    (push dir asdf:*central-registry*)
    (asdf:load-system file)
    file))  ; registered name

(defun qob-find-system (name)
  "Return a system of given NAME."
  (asdf/system-registry:registered-system name))

(defun qob-init-system ()
  "Setup the system."
  (qob-init-ql)
  (let ((files (qob-asd-files t)))
    (mapc (lambda (file)
            (qob-load-system file)
            (qob-info "Load ASD file ~A" file))
          files)))

;;
;;; Externals

;;(qob-load "extern/alexandria")

;;
;;; Initialization

(setq qob-enable-color (not (qob-no-color-p)))

;;; End of lisp/_prepare.lisp
