;;;; lisp/core/package.lisp --- Build a system artifact

;;; Commentary
;;
;; Command use to build a system artifact,
;;
;;   $ qob package
;;

;;; Code

(qob-init-asds)

(ql:quickload "copy-directory" :silent t)

(defun qob-package--build (name system)
  "Build the system artifact."
  (let* ((version (asdf:component-version system))
         (f-name (qob-format "~A-~A" name version))
         (f-dir (qob-expand-fn (qob-format "~A/" f-name) qob-dist-path))
         (root (qob-primary-root))
         (tar (qob-s-remove-suffix "/" (qob-2str f-dir))))
    (ignore-errors (delete-directory f-dir :recursive t))
    (ensure-directories-exist f-dir)

    (dolist (path (qob-system-files))
      (let* ((path (qob-2str path))
             (rel-path (qob-s-replace root "" path))
             (target (qob-expand-fn rel-path f-dir)))
        (cond ((qob-s-slash-p path)
               (copy-directory:copy (qob-expand-fn path) target))
              (t
               (ensure-directories-exist (qob-file-name-directory target))
               (uiop:copy-file path target)))))

    ;; Tar it.
    (qob-msg "")
    (uiop:with-current-directory (qob-dist-path)
      (qob-run-program (list "tar" "-cvf"
                             (qob-format "~A.tar" f-name)
                             (qob-s-remove-prefix
                              (qob-2str qob-dist-path)
                              tar))))
    (qob-msg "")
    (qob-info "(Built in ~A.tar)" tar)))

(let* ((qob-dist-path (or (qob-args 0) qob-dist-path))
       (qob-dist-path (qob-expand-fn qob-dist-path)))
  (ensure-directories-exist qob-dist-path)

  (let ((name   (qob-primary-system-name))
        (system (qob-primary-system)))
    (qob-package--build name system)))

;;; End of lisp/core/package.lisp
