;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defun dot-global ()
  "Return the global .qob directory."
  (el-lib:el-expand-file-name ".qob/" (user-homedir-pathname)))

(defun dot-local ()
  "Return the local .qob directory."
  (el-lib:el-expand-file-name ".qob/"))

(defun lisp-root ()
  "Return the lisp scripts' root directory."
  (el-lib:el-expand-file-name "lisp/" sb-ext:*runtime-pathname*))

(defun setup-environment ()
  "Setup the enviornment variables."
  (setf (uiop:getenv "QOB_DOT_GLOBAL") (dot-global))
  (setf (uiop:getenv "QOB_DOT_LOCAL")  (dot-local))
  (setf (uiop:getenv "QOB_LISP_ROOT")  (lisp-root)))

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

(defun call-script (script &rest options)
  "Run the lisp implementation with the SCRIPT and OPTIONS."
  (let ((prepare (lisp-script "_prepare"))
        (script  (lisp-script script)))
    (apply #'call-impls (list "--load" prepare
                              "--script" script)
           options)))

(defun call-impls (args &rest options)
  "Run the lisp implementation with ARGS and OPTIONS."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    (setup-environment)
    (uiop:run-program (concatenate 'list
                                   (list lisp-impls
                                         "--noinform"
                                         "--userinit" (user-init))
                                   args)
                      :output *standard-output*
                      :error-output *error-output*
                      :force-shell t)))
