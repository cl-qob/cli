;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob-cli)

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

(defun dot ()
  "Return the current .qob directory."
  ;; TODO: Add global/local logic.
  (let ((global-p nil))
    (if global-p (dot-global) (dot-local))))

(defun lisp-root ()
  "Return the lisp scripts' root directory."
  (el-lib:el-expand-file-name "lisp/" (exec-dir)))

(defun setup-environment ()
  "Setup the enviornment variables."
  (setf (uiop:getenv "QOB_LISP")                (program-name))
  (setf (uiop:getenv "QOB_DOT")                 (dot))
  (setf (uiop:getenv "QOB_TEMP_FILE")           (el-lib:el-expand-file-name "tmp" (dot-global)))
  (setf (uiop:getenv "QOB_LISP_ROOT")           (lisp-root))
  (setf (uiop:getenv "QOB_USER_INIT")           (user-init))
  (if (quicklisp-installed-p)
      (setf (uiop:getenv "QOB_QUICKLISP_INSTALLED") "t")
      (quicklisp-download)))

(defun program-name ()
  "Lisp program we target to run."
  (or (uiop:getenv "QOB_LISP")
      "sbcl"))

(defun user-init ()
  "Return the user init file."
  (let ((filename (concatenate 'string (dot-local) "init.lisp")))
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

(defun call-script (script &rest cmd)
  "Run the lisp implementation with the SCRIPT and CMD."
  (let ((prepare (lisp-script "_prepare"))
        (no-ql   (lisp-script "_no_ql"))
        (ql      (lisp-script "_ql"))
        (script  (lisp-script script)))
    (apply #'call-impls
           (concatenate 'list
                        (if (quicklisp-installed-p)
                            (list "--load" no-ql)
                            (list "--load" (quicklisp-lisp)
                                  "--load" ql))
                        (list "--load"   prepare
                              "--script" script))
           cmd)))

(defun call-impls (args &rest cmd)
  "Run the lisp implementation with ARGS and CMD."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    ;; TODO: Turn cmd to file.
    ;;(format t "~A~%" (clingon:command-arguments cmd))
    ;;(format t "~A~%" (clingon:getopt cmd :verbose))
    (setup-environment)
    (uiop:run-program (concatenate 'list
                                   (list lisp-impls)
                                   (list "--noinform"
                                         "--userinit" (user-init))
                                   args)
                      :output :interactive
                      :error-output :interactive
                      :force-shell t)))

;;
;;; Package

(defun quicklisp-lisp ()
  "Return Quicklisp's file location."
  (concatenate 'string (dot) "quicklisp.lisp"))

(defun quicklisp-installed-p ()
  "Return non-nil if Quicklisp is already installed."
  (uiop:file-exists-p (quicklisp-lisp)))

(defun quicklisp-download ()
  "Download quicklisp."
  (uiop:run-program `("curl"
                      "https://beta.quicklisp.org/quicklisp.lisp"
                      "--output"
                      ,(quicklisp-lisp))
                    :output *standard-output*
                    :error-output *error-output*
                    :force-shell t))

;;; End of src/utils.lisp
