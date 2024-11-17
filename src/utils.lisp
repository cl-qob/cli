;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob-cli)

(defvar inhibit-ql-download nil
  "Set to t if you don't  want download `quicklisp.lisp' file on start.")

(defvar force-global-p nil
  "Set to t if force global.")

;;
;;; Flags

(defun global-p (cmd)
  "Non-nil when in global space (`-g', `--global')."
  (or (clingon:getopt cmd :global)
      force-global-p))

(defun local-p (cmd)
  "Non-nil when in local space (default)."
  (not (global-p cmd)))

(defun verbose (cmd)
  "Non-nil when flag has value (`-v', `--verbose')."
  (clingon:getopt cmd :verbose))

;;
;;; Environment

(defun exec-dir ()
  "Return the current executable directory."
  ;; NOTE: `sb-ext:*runtime-pathname*' returns as a pathname.
  (el-lib:el-file-name-directory sb-ext:*runtime-pathname*))

(defun dot-global ()
  "Return the global .qob directory."
  (el-lib:el-expand-file-name ".qob/" (user-homedir-pathname)))

(defun dot-local ()
  "Return the local .qob directory."
  (el-lib:el-expand-file-name ".qob/"))

(defun dot (cmd)
  "Return the current .qob directory."
  (if (global-p cmd) (dot-global) (dot-local)))

(defun lisp-root ()
  "Return the lisp scripts' root directory."
  (el-lib:el-expand-file-name "lisp/" (exec-dir)))

(defun extract-post-arguments (cmd)
  "Extract arguments after two dashes `--' from CMD."
  (let ((args (clingon:command-arguments cmd))
        (pattern "-"))
    (setq args (remove-if-not (lambda (arg)
                                (string= pattern arg :end2 (length pattern)))
                              args))
    args))

(defun prepare-options (cmd)
  "Prepare options string from CMD."
  (let ((opts (list -1)))  ; Assign -1 for `nconc' operation
    ;; Boolean
    (when (global-p cmd)
      (nconc opts `("--global")))
    (when (clingon:getopt cmd :all)
      (nconc opts `("--all")))
    (when (clingon:getopt cmd :no-color)
      (nconc opts `("--no-color")))
    (when (clingon:getopt cmd :compression)
      (nconc opts `("--compression")))
    ;; String (with value)
    (let ((output (clingon:getopt cmd :output)))
      (when output
        (nconc opts `("--output" ,output))))
    ;; Number (with value)
    (let ((verbose (verbose cmd)))
      (when verbose
        (nconc opts `("--verbose" ,(el-lib:el-2str verbose)))))
    (pop opts)  ; pop -1
    opts))

(defun setup-script-env (script)
  "Setup the SCRIPT environment."
  (setf (uiop:getenv "QOB_COMMAND") script))

(defun setup-env (cmd)
  "Setup the enviornment variables.

Argument CMD is used to extract positional arguments and options."
  (ensure-directories-exist (dot cmd))
  (setf (uiop:getenv "QOB_ARGS")      (el-lib:el-2str (clingon:command-arguments cmd)))
  (setf (uiop:getenv "QOB_OPTS")      (el-lib:el-2str (prepare-options cmd)))
  (setf (uiop:getenv "QOB_LISP")      (program-name))  ; Update itself.
  (setf (uiop:getenv "QOB_DOT")       (dot cmd))
  (setf (uiop:getenv "QOB_TEMP_FILE") (el-lib:el-expand-file-name "tmp" (dot-global)))
  (setf (uiop:getenv "QOB_LISP_ROOT") (lisp-root))
  (setf (uiop:getenv "QOB_USER_INIT") (user-init cmd))
  (unless inhibit-ql-download
    (if (quicklisp-installed-p cmd)
        (setf (uiop:getenv "QOB_QUICKLISP_INSTALLED") "t")
        (quicklisp-download cmd))))

(defun program-name ()
  "Lisp program we target to run."
  (or (uiop:getenv "QOB_LISP")
      "sbcl"))

(defun user-init (cmd)
  "Return the user init file."
  (let ((filename (concatenate 'string (dot cmd) "init.lisp")))
    (unless (uiop:file-exists-p filename)
      ;; Ensure the file exists.
      (with-open-file (str filename
                           :direction :output
                           :if-does-not-exist :create)
        ;; Write empty string.
        (format str "")))
    filename))

(defun lisp-script (name)
  "Form lisp script path."
  (let* ((lisp-dir (lisp-root))
         (name (concatenate 'string name ".lisp"))
         (name (el-lib:el-expand-fn name lisp-dir)))
    (namestring name)))

(defun call-script (script cmd)
  "Run the lisp implementation with the SCRIPT and CMD."
  (setup-script-env script)
  (let ((el      (lisp-script "_el_lib"))
        (color   (lisp-script "_color"))
        (prepare (lisp-script "_prepare"))
        (no-ql   (lisp-script "_no_ql"))
        (ql      (lisp-script "_ql"))
        (script  (lisp-script script)))
    (call-impls (concatenate 'list
                             (if (or inhibit-ql-download
                                     (quicklisp-installed-p cmd))
                                 (list "--load" no-ql)
                                 (list "--load" (quicklisp-lisp cmd)
                                       "--load" ql))
                             (list "--load"   el
                                   "--load"   color
                                   "--load"   prepare
                                   "--script" script))
                cmd)))

(defun call-impls (args cmd)
  "Run the lisp implementation with ARGS and CMD."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    (setup-env cmd)
    (let ((command (concatenate 'list
                                (list lisp-impls)
                                (list "--noinform")
                                ;; NOTE: `extract-post-arguments' doesn't work
                                ;; in function `run-program'; add it here?
                                (extract-post-arguments cmd)
                                (when (local-p cmd)
                                  (list "--userinit" (user-init cmd)))
                                args)))
      (when (<= 5 (verbose cmd))
        (format t "~A~%" command))
      (run-program command cmd t))))

(defun run-program (command cmd &optional no-post-args)
  "Return COMMAND program.

If the optional argument NO-POST-ARGS is non nil, do not include post
arguments in the command list."
  (uiop:run-program (concatenate 'list
                                 command
                                 (if no-post-args '()
                                     (extract-post-arguments cmd)))
                    :input :interactive
                    :output :interactive
                    :error-output :interactive
                    :force-shell t))

;;
;;; Package

(defun quicklisp-lisp (cmd)
  "Return Quicklisp's file location."
  (if (global-p cmd)
      (concatenate 'string (el-lib:el-2str (user-homedir-pathname))
                   "quicklisp/")
      (concatenate 'string (dot cmd) "quicklisp.lisp")))

(defun quicklisp-installed-p (cmd)
  "Return non-nil if Quicklisp is already installed."
  (probe-file (quicklisp-lisp cmd)))

(defun quicklisp-download (cmd)
  "Download quicklisp."
  (run-program `("curl"
                 "https://beta.quicklisp.org/quicklisp.lisp"
                 "--output"
                 ,(quicklisp-lisp cmd))
               cmd t))

;;; End of src/utils.lisp
