;;; lisp/_prepare.lisp --- Prepare for command tasks
;;; Commentary: Prepare to setup Qob environment for sandboxing
;;; Code:

;;
;;; Includes

(require "asdf")

;;
;;; Utils

(defmacro qob-ignore-errors (&rest forms)
  "Ignore errors."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(defmacro qob-silent (&rest body)
  "Execute BODY without output."
  `(with-open-stream (*standard-output* (make-broadcast-stream)) ,@body))

(defun qob-format (string &rest objects)
  "Format string."
  (apply #'qob-el-format string objects))

(defun qob-2str (object)
  "Convert to string."
  (funcall #'qob-el-2str object))

(defun qob-s-replace (old new str)
  "Replaces OLD with NEW in S."
  (let ((pos (search old str)))
    (if pos
        (concatenate 'string
                     (subseq str 0 pos)
                     new
                     (subseq str (+ pos (length old))))
        str)))  ; Return original if substring not found

(defun qob-file-get-lines (filename)
  "Get FILENAME's contents in list of lines."
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun qob-file-get-contents (filename)
  "Get FILENAME's contents in string."
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun qob--sinr (len-or-list form-1 form-2)
  "If LEN-OR-LIST has length of 1; return FORM-1, else FORM-2."
  (let ((len (if (numberp len-or-list) len-or-list (length len-or-list))))
    (if (<= len 1) form-1 form-2)))

;;
;;; Color

(defun qob--msg-paint-kwds (string)
  "Paint keywords from STRING."
  (let* ((string (qob-s-replace "✓" (qob-ansi-green "✓") string))
         (string (qob-s-replace "✗" (qob-ansi-red "✗") string))
         (string (qob-s-replace "💡" (qob-ansi-yellow "💡") string)))
    string))

(defun qob--format-paint-kwds (msg &rest args)
  "Paint keywords after format MSG and ARGS."
  (let* ((string (apply #'qob-el-format msg args))
         (string (qob--msg-paint-kwds string)))
    string))

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
    (apply #'format stream fmt args)
    (force-output stream)))

(defun qob-print (msg &rest args)
  "Standard output print MSG and ARGS."
  (qob-princ 'stdout (apply #'qob--format-paint-kwds msg args)))

(defun qob-println (msg &rest args)
  "Like function `qob-print' but with newline at the end."
  (apply #'qob-print (concatenate 'string msg "~%") args))

(defun qob-write (msg &rest args)
  "Standard error print MSG and ARGS."
  (qob-princ 'stderr (apply #'qob--format-paint-kwds msg args)))

(defun qob-msg (msg &rest args)
  "Standard error print line MSG and ARGS."
  (apply #'qob-write (concatenate 'string msg "~%") args))

(defun qob-trace (msg &rest args)
  "Send trace message; see function `qob--msg' for arguments MSG and ARGS."
  (let ((msg (apply #'format nil msg args)))
    (qob-msg (qob-ansi-white msg))))

(defun qob-debug (msg &rest args)
  "Send debug message; see function `qob--msg' for arguments MSG and ARGS."
  (let ((msg (apply #'format nil msg args)))
    (qob-msg (qob-ansi-blue msg))))

(defun qob-info (msg &rest args)
  "Send info message; see function `qob--msg' for arguments MSG and ARGS."
  (let ((msg (apply #'format nil msg args)))
    (qob-msg (qob-ansi-cyan msg))))

(defun qob-warn (msg &rest args)
  "Send warning message; see function `qob--msg' for arguments MSG and ARGS."
  (let ((msg (apply #'format nil msg args)))
    (qob-msg (qob-ansi-yellow msg))))

(defun qob-error (msg &rest args)
  "Send error message; see function `qob--msg' for arguments MSG and ARGS."
  (let ((msg (apply #'format nil msg args)))
    (qob-msg (qob-ansi-red msg)))
  (uiop:quit 1))

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

(defun qob-dot-impls ()
  "Return the directory path to `.qob/type/version'.

For example, `.qob/sbcl/2.4.9/'."
  (uiop:merge-pathnames* (concatenate 'string
                                      (lisp-implementation-type) "/"
                                      (lisp-implementation-version) "/")
                         qob-dot))

;;
;;; Help

(defun qob--help-display (lines)
  "Display help instruction.

The argument LINES is the content strings."
  (let ((max-width 0)
        (section-sep))
    (dolist (line lines)
      (setq max-width (max max-width (length line))))
    (setq section-sep
          (concatenate 'string
                       "''" (make-string max-width :initial-element #\Space) "''"))
    (qob-msg section-sep)
    (dolist (line lines)
      (qob-msg "  ~A  " (qob-ansi-white line)))
    (qob-msg "")
    (qob-msg section-sep)))

(defun qob-help (command)
  "Show COMMAND's help instruction."
  (let* ((command (qob-2str command))  ; convert to string
         (help-file (concatenate 'string qob-lisp-root "help/" command)))
    (if (uiop:file-exists-p help-file)
        (let ((lines (qob-file-get-lines help-file)))
          (qob--help-display lines))
        (qob-error "Help manual missig %s" help-file))))

;;
;;; Load file

(defvar qob-loading-file-p nil
  "This became t; if we are loading script from another file and not expecting
the `qob-start' execution.")

(defun qob-script (script)
  "Return full SCRIPT filename."
  (concatenate 'string qob-lisp-root script ".lisp"))

(defun qob-call (script)
  "Call another qob SCRIPT."
  (let ((script-file (qob-script script)))
    (if (uiop:file-exists-p script-file)
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
;;; Verbose

(defun qob--verb2lvl (symbol)
  "Convert verbosity SYMBOL to level."
  (case symbol
    (all   5)
    (debug 4)
    (log   3)
    (info  2)
    (warn  1)
    (error 0)
    (t symbol)))

(defun qob-reach-verbosity-p (symbol)
  "Return t if SYMBOL reach verbosity (should be printed)."
  (>= (qob-verbose) (qob--verb2lvl symbol)))

(defmacro qob-with-verbosity (symbol &rest body)
  "Define verbosity scope.

Execute forms BODY limit by the verbosity level (SYMBOL)."
  `(if (qob-reach-verbosity-p ,symbol)
       (progn ,@body)
       (qob-silent ,@body)))

;;
;;; Progress

(defmacro qob-with-progress (msg-start body msg-end)
  "Progress BODY wrapper with prefix (MSG-START) and suffix (MSG-END) messages."
  `(progn (qob-write ,msg-start) ,body (qob-msg ,msg-end)))

;;
;;; ASDF

(defun qob-system-version (name)
  "Get the system version."
  (let ((system (asdf:find-system name nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

;;
;;; Package

(defconstant qob-source-mapping
  `((quicklisp . "http://beta.quicklisp.org/")
    (ultralisp . "http://dist.ultralisp.org/"))
  "Mapping of source name and url.")

(defvar qob-ql-init-p nil
  "Set to t when QuickLisp is initialized.")

(defun qob-ql-installed-dir ()
  "Return the QuickLisp installed directory base on scope."
  (uiop:merge-pathnames* "quicklisp/" (if (qob-global-p)
                                          (user-homedir-pathname)
                                          (qob-dot-impls))))

(defun qob-init-ql (&optional force)
  "Initialize QuickLisp."
  (when (or (not qob-ql-init-p)
            force)
    (qob-with-progress
     (qob-ansi-green "Setting up QuickLisp... ")
     (let* ((ql-dir (qob-ql-installed-dir))
            (ql-init (uiop:merge-pathnames* "setup.lisp" ql-dir)))
       (unless qob-quicklisp-installed-p
         (qob-quicklisp-install ql-dir))
       (when (probe-file ql-init)
         (load ql-init)))
     (qob-ansi-green "done ✓"))
    (setq qob-ql-init-p t)))

;;
;;; ASDF file

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

(defvar qob-asds-init-p nil
  "Set to t when ASDF files are initialized.")

(defvar qob-loaded-asds nil
  "Loaded ASD files.")

(defun qob-init-asds (&optional force)
  "Initialize the ASD files.

This function only loads the ASD file but doesn't actually try to
set up the system.  You should use the function `qob-init-systems'
to actually set up the systems."
  (when (and (qob-local-p)
             (or (not qob-asds-init-p)
                 force))
    (setq qob-loaded-asds nil)  ; reset
    (qob-with-progress
     (qob-ansi-green "Loading ASDF files... ")
     (qob-with-verbosity
      'debug
      (let ((files (qob-asd-files t))
            (pre-systems (asdf:registered-systems)))
        (mapc (lambda (file)
                (asdf:load-asd file)
                (qob-println "Loaded ASD file ~A" file))
              files)
        (setq qob-loaded-asds
              (remove-if (lambda (system)
                           (qob-el-memq system pre-systems))
                         (asdf:registered-systems)))))
     (qob-ansi-green "done ✓"))
    (setq qob-asds-init-p t)))

(defun qob-only-system ()
  "Return the default system if only one system is loaded in the workspace."
  (qob-init-asds)
  (when (= (length qob-loaded-asds) 1)
    (nth 0 qob-loaded-asds)))

;;
;;; ASDF system

(defvar qob-systems-init-p nil
  "Set to t when system is initialized.")

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

(defvar qob-loaded-systems nil
  "List of loaded systems.")

(defun qob-init-systems (&optional force)
  "Initialize the ASD systems.

Set up the systems; on contrary, you should use the function
`qob-init-asds' if you only want the ASD files to be loaded."
  (when (and (qob-local-p)
             (or (not qob-systems-init-p)
                 force))
    (setq qob-loaded-systems nil)  ; reset
    (qob-with-progress
     (qob-ansi-green "Loading ASDF systems... ")
     (qob-with-verbosity
      'debug
      (let ((files (qob-asd-files t)))
        (mapc (lambda (file)
                (push (qob-load-system file) qob-loaded-systems)
                (qob-println "Loaded system file ~A" file))
              files)))
     (qob-ansi-green "done ✓"))
    (setq qob-systems-init-p t)))

;;
;;; Externals

;;(qob-load "extern/alexandria")

;;
;;; Initialization

(setq qob-enable-color (not (qob-no-color-p)))

;;; End of lisp/_prepare.lisp
