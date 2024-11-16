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
         (components (asdf:component-children system))
         (f-name (qob-format "~A-~A" name version))
         (f-dir (qob-expand-fn (qob-format "~A/" f-name) qob-dist-path)))
    (ensure-directories-exist f-dir)

    (dolist (comp components)
      (let ((file (ignore-errors (asdf:component-pathname comp))))
        (when file
          (qob-info "? ~A" file)
          (uiop:copy-file file f-dir)
          )
        ))


    ;;(qob-run-program '("tar" ))
    ))

(let* ((qob-dist-path (or (qob-args 0) qob-dist-path))
       (qob-dist-path (qob-expand-fn qob-dist-path)))
  (ensure-directories-exist qob-dist-path)

  (let* ((name   (qob-primary-system-name))
         (system (asdf:find-system name)))
    (qob-package--build name system)))

;;; End of lisp/core/package.lisp
