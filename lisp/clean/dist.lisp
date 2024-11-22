;;;; lisp/clean/dist.lisp --- Clean up dist directory

;;; Commentary
;;
;; Command use to clean up dist directory
;;
;;   $ qob clean dist
;;

;;; Code

(defun qob-clean-dist ()
  "Clean up the dist files."
  (let* ((name   (qob-primary-system-name))
         (system (qob-primary-system))
         (version (asdf:component-version system))
         (f-name (qob-format "~A-~A" name version))
         (f-tar (qob-expand-fn (qob-format "~A.tar" f-name) qob-dist-path))
         (deleted 0)
         (delete-dir nil))
    (when (probe-file f-tar)
      (ignore-errors (delete-file f-tar))
      (incf deleted))
    (setq delete-dir (ignore-errors (uiop:delete-empty-directory qob-dist-path)))
    (qob-msg "")
    (qob-info "(Total of ~A file~A and ~A directory deleted)" deleted
              (qob--sinr deleted "" "s")
              (if delete-dir "1" "0"))))

(qob-start
 (let ((qob-dist-path (qob-dist-path)))
   (if (probe-file qob-dist-path)
       (qob-clean-dist)
       (progn
         (qob-info "(No dist folder needs to be cleaned)" qob-dist-path)
         (setq qob-no-cleaning-operation-p t)))))

;;; End of lisp/clean/dist.lisp
