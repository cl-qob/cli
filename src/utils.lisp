;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defvar *dot-root* ".qob/"
  "The .qob directory.")

(defvar *lisp-root* "lisp/"
  "Directory path points to the lisp folder.")

(defun program-name ()
  "Lisp program we target to run."
  (or (uiop:getenv "QOB_LISP")
      "sbcl"))

(defun user-init ()
  "Return the user init file."
  (let ((filename (concatenate 'string *dot-root* "init.lisp")))
    (unless (uiop:file-exists-p filename)
      ;; Ensure the file exists.
      (with-open-file (str filename
                           :direction :output
                           :if-does-not-exist :create)
        (format str "")))
    filename))

(defun lisp-script (name)
  "Form lisp script path."
  (let* ((lisp-dir (el-lib:el-expand-fn *lisp-root* sb-ext:*runtime-pathname*))
         (name (concatenate 'string name ".lisp"))
         (name (el-lib:el-expand-fn name lisp-dir)))
    (namestring name)))

(defun call-script (script &rest options)
  "Run the lisp implementation with the SCRIPT and OPTIONS."
  (let ((prepare (lisp-script "_prepare"))
        (script  (lisp-script script)))
    (apply #'call-impls (list "--load" prepare
                              "--load" script)
           options)))

(defun call-impls (args &rest options)
  "Run the lisp implementation with ARGS and OPTIONS."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    (uiop:run-program (concatenate 'list
                                   (list lisp-impls
                                         "--userinit" (user-init))
                                   args)
                      :output t
                      :force-shell t)))
