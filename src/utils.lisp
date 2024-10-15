;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defvar *lisp-root* "lisp/"
  "Directory path points to the lisp folder.")

(defun program-name ()
  "Lisp program we target to run."
  (or (uiop:getenv "QOB_LISP")
      "sbcl"))

(defun lisp-script (name)
  "Form lisp script path."
  (let* ((lisp-dir (el-lib:el-expand-fn *lisp-root* sb-ext:*runtime-pathname*))
         (name (concatenate 'string name ".lisp"))
         (name (el-lib:el-expand-fn name lisp-dir)))
    (namestring name)))

(defun call-lisp (script &rest args)
  "Run the lisp implementation."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    (let ((prepare (lisp-script "_prepare"))
          (script  (lisp-script script)))
      (uiop:run-program (list lisp-impls
                              "--load" prepare
                              "--load" script)
                        :output t
                        :force-shell t))))
