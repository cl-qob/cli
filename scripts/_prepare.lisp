;;;; scripts/_prepare.lisp --- Some utilities
;;; Commentary
;;; Code

(defun qob-delete-exec ()
  "Delete the qob executable."
  ;; Delete executable
  (let ((exec (el-lib:el-expand-fn (if (uiop:os-windows-p)
                                       "bin/qob.exe"
                                       "bin/qob"))))
    (when (uiop:file-exists-p exec)
      (delete-file exec))))

;;; End of scripts/prepare.lisp
