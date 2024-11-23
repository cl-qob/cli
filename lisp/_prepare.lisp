;;; lisp/_prepare.lisp --- Prepare for command tasks
;;; Commentary: Prepare to setup Qob environment for sandboxing
;;; Code:

;;
;;; Includes

(require "asdf")

(defconstant qob-homepage "https://cl-qob.github.io/"
  "Qob's home page.")

;;
;;; Variables

(defvar qob-files nil)
(defvar qob-depends-on nil)

(defvar qob-before-command-hook nil)
(defvar qob-after-command-hook nil)

;;
;;; Utils

(defmacro qob-silent (&rest body)
  "Execute BODY without output."
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     (with-open-stream (*error-output* (make-broadcast-stream)) ,@body)))

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

(defun qob-eval (str)
  "Evaluate a STR."
  (let ((*readtable* (copy-readtable *readtable*)))
    (setf (readtable-case *readtable*) :preserve)
    (eval (read-from-string str))))

(defun qob-run-program (command)
  "Return COMMAND program."
  (uiop:run-program command
                    :input :interactive
                    :output :interactive
                    :error-output :interactive
                    :force-shell t))

(defun qob-expand-file-specs (specs)
  "Expand file SPECS."
  (let ((temp-files))
    (mapcar (lambda (spec)
              (setq temp-files (append temp-files
                                       (directory spec))))
            specs)
    temp-files))

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
  (let* ((string (apply #'qob-format msg args))
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
;;; Input

(defun qob-read-line (prompt &optional default-value)
  "Like `read-line'."
  (let* ((prompt (if default-value (qob-format "~A(~A) " prompt default-value)
                     prompt))
         (answer))
    (qob-print prompt)
    (setq answer (read-line))
    (if (and default-value (string= "" answer))
        default-value
        answer)))

;;
;;; Environment

(defun qob-parse-args (env-name)
  "Parse arguments.

Argument ENV-NAME is used to get the argument string."
  (let* ((args (uiop:getenv env-name))
         (args (concatenate 'string "'" args)))
    (mapcar #'qob-2str (qob-eval args))))

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

(defvar qob-command (uiop:getenv "QOB_COMMAND")
  "Hold the current command.")

(defvar qob-quicklisp-installed-p (uiop:getenv "QOB_QUICKLISP_INSTALLED")
  "Return non-nil if Quicklisp is already installed.")

(defvar qob-inhibit-ql-download-p (uiop:getenv "QOB_INHIBIT_QL_DOWNLOAD")
  "Return non-nil if Quicklisp is inhibit to be downloaded.")

(defun qob-dot-impls ()
  "Return the directory path to `.qob/type/version'.

For example, `.qob/sbcl/2.4.9/'."
  (uiop:merge-pathnames* (concatenate 'string
                                      (lisp-implementation-type) "/"
                                      (lisp-implementation-version) "/")
                         qob-dot))

(defun qob-args (&optional n)
  "Return the program arguments.

If optional argument is non-nil, return the N th argument instead."
  (if n (nth n qob-args) qob-args))

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

(defun qob-compression-p ()
  "Non-nil when flag is on (`--compression')."
  (if (qob--flag "--compression") t nil))

;;; String (with arguments)
(defun qob-output ()
  "Non-nil when flag has value (`--o', `--output')."
  (qob--flag-value "--output"))

;;; Number (with arguments)
(defun qob-verbose ()
  "Non-nil when flag has value (`-v', `--verbose')."
  (parse-integer (qob--flag-value "--verbose")))

;;
;;; Execution

(defun qob-command ()
  "Return the current command."
  (qob-s-replace "core/" "" qob-command))

(defun qob-command-p (commands)
  "Return t if COMMANDS is the current command."
  (qob-member (qob-command) (qob-listify commands)))

(defun qob-special-p ()
  "Return t if the command that can be run without Qob-file existence.

These commands will first respect the current workspace.  If the current
workspace has no valid Qob-file; it will load global workspace instead."
  (qob-command-p '("init"
                   "create/cl-project")))

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

(defvar qob--ql-init-p nil
  "Set to t when Quicklisp is initialized.")

(defun qob-ql-installed-dir ()
  "Return the Quicklisp installed directory base on scope."
  (uiop:merge-pathnames* "quicklisp/" (if (qob-global-p)
                                          (user-homedir-pathname)
                                          (qob-dot-impls))))

(defun qob-ql-local-dir ()
  "Return the Quicklisp local project directory."
  (uiop:merge-pathnames* "local-projects/" (qob-ql-installed-dir)))

(defun qob-init-ql (&optional force)
  "Initialize Quicklisp."
  (when (or (not qob--ql-init-p)
            force)
    (qob-with-progress
     (qob-ansi-green "Setting up Quicklisp... ")
     (let* ((ql-dir (qob-ql-installed-dir))
            (ql-setup (uiop:merge-pathnames* "setup.lisp" ql-dir)))
       (unless qob-quicklisp-installed-p
         (qob-quicklisp-install ql-dir))
       (when (probe-file ql-setup)
         (load ql-setup)))
     (qob-ansi-green "done ✓"))
    (setq qob--ql-init-p t)))

;; Must include!
(qob-init-ql)

(defun qob-ql-no-dists ()
  "Disable all dists."
  (mapc (lambda (dist) (ql-dist:disable dist)) (ql-dist:all-dists)))

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
    (remove-if (lambda (filename) (qob-memq filename tests)) files)
    files))

(defvar qob--asds-init-p nil
  "Set to t when ASDF files are initialized.")

(defvar qob-loaded-asds nil
  "Loaded ASD files.

The cons looks like `((name . path) (name . path)).")

(defun qob-newly-loaded-systems (pre-systems)
  "Return the a newly loaded systems compare to PRE-SYSTESM."
  (remove-if (lambda (system)
               (qob-memq system pre-systems))
             (asdf:registered-systems)))

(defun qob-init-asds (&optional force)
  "Initialize the ASD files.

This function only loads the ASD file but doesn't actually try to
set up the system.  You should use the function `qob-init-systems'
to actually set up the systems."
  (when (and (qob-local-p)
             (or (not qob--asds-init-p)
                 force))
    (setq qob-loaded-asds nil)  ; reset
    (qob-with-progress
     (qob-ansi-green "Loading ASDF files... ")
     (qob-with-verbosity
      'debug
      (let ((files (qob-asd-files t)))
        (mapc (lambda (file)
                (let ((pre-systems (asdf:registered-systems)))
                  (asdf:load-asd file)
                  (qob-println "Loaded ASD file ~A" file)
                  (dolist (new-system (qob-newly-loaded-systems pre-systems))
                    (push (cons new-system file) qob-loaded-asds))))
              files)))
     (qob-ansi-green "done ✓"))
    (setq qob--asds-init-p t)))

(defun qob-primary-system-name ()
  "Return the primary system name."
  (qob-init-asds)
  (unless qob-loaded-asds
    (qob-error "There is no specified ASDF system"))
  (some (lambda (asd)
          (asdf:primary-system-name (car asd)))
        qob-loaded-asds))

(defun qob-primary-test-system-name ()
  "Return the primary test system name."
  (let* ((name (qob-primary-system-name))
         (name (concatenate 'string name "/tests")))
    name))

(defun qob-primary-system-entry ()
  "Return the primary system entry."
  (let ((name (qob-primary-system-name)))
    ;; NOTE: Not sure why function `assoc' isn't working here;
    ;; use some and return the value instead.
    (some (lambda (asd)
            (when (equal (car asd) name)
              asd))
          qob-loaded-asds)))

(defun qob-primary-test-system-entry ()
  "Return the primary test system entry."
  (let ((name (qob-primary-test-system-name)))
    ;; NOTE: Not sure why function `assoc' isn't working here;
    ;; use some and return the value instead.
    (some (lambda (asd)
            (when (equal (car asd) name)
              asd))
          qob-loaded-asds)))

(defun qob-primary-system ()
  "Return the primary system."
  (let ((name (qob-primary-system-name)))
    (asdf:find-system name)))

(defun qob-primary-test-system ()
  "Return the primary test system."
  (let ((name (qob-primary-test-system-name)))
    (asdf:find-system name)))

;; NOTE: Use this as project root?
(defun qob-primary-root ()
  "Return the primary system path."
  (let ((path (cdr (qob-primary-system-entry))))
    (qob-file-name-directory path)))

(defun qob-find-asd-file (name)
  "Return the ASD file by system's NAME."
  (let ((result))
    (dolist (system qob-loaded-asds)
      (when (equal name (car system))
        (setq result (cdr system))))
    result))

(defun qob-system-files ()
  "Return a list of system files."
  (let* ((system (qob-primary-system))
         (components (asdf:component-children system))
         (files))
    (setq files
          (concatenate
           'list
           (mapcar (lambda (component)
                     (let ((file (ignore-errors (asdf:component-pathname component))))
                       (when file
                         (qob-2str file))))
                   components)
           (directory "*.asd")
           (directory "*.lisp")
           (qob-expand-file-specs qob-files)))
    files))

;;
;;; ASDF system

(defvar qob--systems-init-p nil
  "Set to t when system is initialized.")

(defun qob-load-system (filename)
  "Load the system from ASD's FILENAME; and return the registered name."
  (let ((dir (qob-file-name-directory filename))
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
             (or (not qob--systems-init-p)
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
    (setq qob--systems-init-p t)))

;;
;;; Qob file

(defvar qob-file nil
  "Read the Qob file.")

(defun qob-file-load (dir)
  "Load the Qob file in DIR."
  (setq qob-file nil)  ; reset
  (let ((file (qob-locate-dominating-file "Qob" dir)))
    (when file
      (load file)
      (setq qob-file file)))
  qob-file)

;;
;;; DSL

(defun files (&rest spec)
  "Include file spec."
  (setq qob-files (append qob-files
                          spec)))

(defun source (name &optional location)
  "Add dist NAME with LOCATION."
  (let ((dist (ql-dist:find-dist name))
        (url (or location
                 (cdr (assoc (read-from-string name) qob-source-mapping)))))
    (if dist
        (ql-dist:enable dist)
        (if url (ql-dist:install-dist url :prompt nil)
            (qob-error "Can't find dist with name: ~A" name)))))

(defun depends-on (&rest args)
  "Define a local systems"
  (push args qob-depends-on))

;;
;;; User customization

(defvar qob-dist-path "dist/"
  "Default path where to place the package artifact.")

(defun qob-dist-path ()
  "Path to dist path."
  (let ((path (or (qob-args 0) qob-dist-path)))
    (qob-expand-fn path)))

;;
;;; Entry

(defun qob-add-hook (hook function)
  "Add FUNCTION to the HOOK list if not already present."
  (set hook nil)  ; Declare it.
  (unless (member function (symbol-value hook))
    (push function (symbol-value hook))))

(defun qob-run-hooks (hooks)
  "Run all functions in the HOOKS list."
  (dolist (fn (ignore-errors (symbol-value hooks)))
    (funcall fn)))

(defun qob--form-command-var (name)
  "From the command variable by NAME."
  (let* ((name (string-upcase name))
         (name (intern name)))
    name))

(defun qob--before-command-var ()
  "Return the before command name."
  (let* ((command (qob-command))
         (before  (concatenate 'string "qob-before-" command "-hook")))
    (qob--form-command-var before)))

(defun qob--after-command-var ()
  "Return the after command name."
  (let* ((command (qob-command))
         (after   (concatenate 'string "qob-after-" command "-hook")))
    (qob--form-command-var after)))

;; Declare variables
(let ((before (qob--before-command-var))
      (after  (qob--after-command-var)))
  (set before nil)
  (set after nil))

(defun qob--with-hooks (body)
  "Execute BODY with before/after hooks."
  (let ((before  (qob--before-command-var))
        (after   (qob--after-command-var)))
    (qob-run-hooks 'qob-before-command-hook)
    (qob-run-hooks before)
    (funcall body)
    (qob-run-hooks after)
    (qob-run-hooks 'qob-after-command-hook)))

(defmacro qob-start (&rest body)
  "Execute BODY with workspace setup."
  `(qob--with-hooks (lambda () ,@body)))

;;
;;; Initialization

(ensure-directories-exist (qob-dot-impls))

(setq qob-enable-color (not (qob-no-color-p)))

(cond ((qob-global-p)
       ;; Load configuration.
       (qob-with-verbosity
        'debug (if (qob-file-load (user-homedir-pathname))
                   (qob-msg "✓ Loading global Qob file in ~A... done!" qob-file)
                   (qob-msg "✗  Loading global Qob file... missing!"))))
      ((qob-special-p)
       ;; Load configuration.
       (qob-with-verbosity
        'debug (let ((scope ""))
                 (if (qob-file-load (uiop:getcwd))
                     (setq scope "global ")
                     (qob-file-load (user-homedir-pathname)))
                 (if qob-file
                     (qob-msg "✓ Loading ~AQob file in ~A... done!" scope qob-file)
                     (qob-msg "✗ Loading Qob file... missing!")))))
      (t
       ;; All dists are disabled be default.
       (qob-ql-no-dists)

       ;; Load configuration.
       (qob-with-verbosity
        'debug (if (qob-file-load (uiop:getcwd))
                   (qob-msg "✓ Loading Qob file in ~A... done!" qob-file)
                   (qob-msg "✗ Loading Qob file... missing!")))))

;;
;;; Commad variables

(defvar qob-no-cleaning-operation-p nil
  "Set to non-nil if there is no cleaning operation done.")

;;; End of lisp/_prepare.lisp
