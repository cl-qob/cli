;;;; scripts/_prepare.lisp --- Some utilities
;;; Commentary
;;; Code

(require 'asdf)

(load "~/quicklisp/setup.lisp")
(ql:quickload "deploy")
(ql:quickload "clingon")
(ql:quickload "copy-directory")

(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "qob-cli")
(asdf:load-system "copy-directory")

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
