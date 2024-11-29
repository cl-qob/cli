;;;; scripts/_prepare.lisp --- Some utilities
;;; Commentary
;;; Code

(require 'asdf)

(load "~/quicklisp/setup.lisp")

(defun this-load-system (name)
  "Load the system by NAME."
  (if (asdf:find-system name)
      (asdf:load-system name)
      (ql:quickload name)))

(this-load-system "deploy")
(this-load-system "clingon")
(this-load-system "copy-directory")

(push '*default-pathname-defaults* asdf:*central-registry*)
(this-load-system "qob-cli")
(this-load-system "copy-directory")

;;
;;; Util

(defun qob-copy-lisp-dir ()
  "Copy `lisp' directory over."
  (when (probe-file "bin/lisp/")
    (delete-directory "bin/lisp/" :recursive t))
  (copy-directory:copy (el-lib:el-expand-fn "lisp/")
                       (el-lib:el-expand-fn "bin/lisp/")))

(defun qob-delete-exec ()
  "Delete the qob executable."
  ;; Delete executable
  (let ((exec (el-lib:el-expand-fn (if (uiop:os-windows-p)
                                       "bin/qob.exe"
                                       "bin/qob"))))
    (when (uiop:file-exists-p exec)
      (delete-file exec))))

;;; End of scripts/prepare.lisp
