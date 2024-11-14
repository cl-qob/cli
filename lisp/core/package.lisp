;;;; lisp/core/package.lisp --- Build a system artifact

;;; Commentary
;;
;; Command use to build a system artifact,
;;
;;   $ qob package
;;

;;; Code

(qob-init-asds)

(ql:quickload "cl-autorepo" :silent t)

;; Install QL controller
(let ((cl-autorepo::*repo-dir* (qob-ql-local-dir)))
  (cl-autorepo:add-system "githappy" "https://github.com/xach/githappy" :git)
  (cl-autorepo:add-system "project-info" "https://github.com/quicklisp/project-info" :git)
  (cl-autorepo:add-system "quicklisp-controller" "https://github.com/quicklisp/quicklisp-controller" :git))

(ql:quickload "quicklisp-controller" :silent t)

;; Build the artifact.
(let ((fs (make-instance 'quicklisp-controller:single-file-source)))
  (quicklisp-controller:make-release-tarball
   fs
   (make-pathname :name (project-name source)
                  :type "tgz"
                  :defaults "./")))

;;; End of lisp/core/package.lisp
