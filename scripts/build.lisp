;;;; scripts/build.lisp --- Build the source to executable

;;; Commentary
;;
;; NOTE: This will soon be replace with this build tools!
;;

;;; Code

(require 'asdf)

(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "qob")

;;(ql:quickload "cl-autorepo")
;;(ql:quickload "clingon")

;;(load "./src/build.lisp")
;;(load "./src/main.lisp")

;;(setq uiop:*image-entry-point* #'qob:main)

;;(uiop:dump-image "./bin/qob.exe" :executable t)

(el-lib:el-copy-directory "lisp/" "bin/lisp/")

(let ((exec (el-lib:el-expand-fn (if (uiop:os-windows-p)
                                     "bin/qob.exe"
                                     "bin/qob"))))
  (when (uiop:file-exists-p exec)
    (delete-file exec)))

(asdf:operate :build-op "qob")
