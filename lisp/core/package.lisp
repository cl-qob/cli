;;;; lisp/core/package.lisp --- Build a system artifact

;;; Commentary
;;
;; Command use to build a system artifact,
;;
;;   $ qob package
;;

;;; Code

;; Must be able to build.
(qob-init-systems)

(defun qob-package--tar (output)
  "Build system to artifact."

  )

;; Build the artifact.
(let ((fs (make-instance 'quicklisp-controller:single-file-source)))
  (quicklisp-controller:make-release-tarball
   fs
   (make-pathname :name (project-name source)
                  :type "tgz"
                  :defaults "./")))

(let ((qob-dist-path (or (qob-args 0) qob-dist-path)))
  )

;;; End of lisp/core/package.lisp
