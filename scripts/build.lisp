;;;; scripts/build.lisp --- Build the source to executable

;;; Commentary
;;
;; NOTE: This will soon be replace with this build tools!
;;

;;; Code

(require 'asdf)

(when (uiop:getenv "GITHUB_WORKSPACE")
  ;;(ql:quickload "cl-autorepo")
  (ql:quickload "clingon")
  (ql:quickload "copy-directory"))

(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "qob-cli")
(asdf:load-system "copy-directory")

;;; Copy lisp directory
(progn
  (when (uiop:probe-file exec)
    (el-lib:el-delete-directory "bin/lisp/"))
  (copy-directory:copy (el-lib:el-expand-fn "lisp/")
                       (el-lib:el-expand-fn "bin/lisp/")))

;; Delete executable
(let ((exec (el-lib:el-expand-fn (if (uiop:os-windows-p)
                                     "bin/qob.exe"
                                     "bin/qob"))))
  (when (uiop:file-exists-p exec)
    (delete-file exec)))

;; Build executable
(asdf:operate :build-op "qob-cli")
