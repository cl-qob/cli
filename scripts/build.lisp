;;;; scripts/build.lisp --- Build the source to executable

;;; Commentary
;;
;; NOTE: This will soon be replace with this build tools!
;;

;;; Code

(load "scripts/_prepare.lisp")

(qob-copy-lisp-dir)
(qob-delete-exec)

;; Build executable
;;(asdf:make "qob-cli" :compression nil)

;; (copy-directory:copy (el-lib:el-expand-fn "lisp/")
;;                      (el-lib:el-expand-fn "dist/qob-cli-0.1.0/lisp/"))

(sb-ext:save-lisp-and-die (if (uiop:os-windows-p)
                              "bin/qob.exe"
                              "bin/qob")
                          :purify t
                          :compression nil
                          :toplevel #'qob-cli:main
                          :save-runtime-options t
                          :executable t)

;;; End of scripts/build.lisp
