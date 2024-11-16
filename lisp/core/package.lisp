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

(defun qob-package--tar (output)
  "Build system to artifact."
  )

(let ((qob-dist-path (or (qob-args 0) qob-dist-path)))
  (ensure-directories-exist qob-dist-path)

  (unless qob-loaded-asds
    (qob-error "There is no specified ASD system"))

  (qob-info "? ~A" (qob-primary-system))

  (let* ((name    (car (nth 0 qob-loaded-asds)))
         (system  (asdf:find-system name))
         (version (asdf:component-version system))
         (files   (asdf:component-file)))
    ;;(qob-run-program '("tar" ))
    )
  )

;;; End of lisp/core/package.lisp
