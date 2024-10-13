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

(defun copy-directory (src-dir dst-dir)
  "Recursively copy the contents of SRC-DIR to DST-DIR."
  (let ((src (uiop:parse-native-namestring src-dir))
        (dst (uiop:parse-native-namestring dst-dir)))
    ;; Ensure the source directory exists
    (unless (probe-file src)
      (error "Source directory ~a does not exist!" src))

    ;; Create destination directory if it doesn't exist
    (unless (probe-file dst)
      (ensure-directories-exist dst))

    ;; Recursively copy files and directories
    (dolist (entry (directory (merge-pathnames "*" src)))
      (let ((entry-name (uiop:parse-native-namestring entry)))
        (if (uiop:file-exists-p entry-name)
            ;; Copy file
            (uiop:copy-file entry-name (merge-pathnames (uiop:pathname-name entry) dst))
            ;; Recursively copy subdirectory
            (copy-directory entry-name (merge-pathnames (uiop:pathname-name entry) dst)))))))

(copy-directory "lisp" "bin/lisp")

(let ((exec (el-lib:el-expand-fn (if (uiop:os-windows-p)
                                     "bin/qob.exe"
                                     "bin/qob"))))
  (when (uiop:file-exists-p exec)
    (delete-file exec)))

(asdf:operate :build-op "qob")
