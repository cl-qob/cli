;;;; lisp/core/build.lisp --- Build executable

;;; Commentary
;;
;; Command use to build the executable
;;
;;   $ qob build
;;
;;
;;  Optional arguments:
;;
;;    --name, -n         path to the ASD file
;;    --output, -o       output directory
;;

;;; Code

(qob-init-system)

(let ((names qob-args))
  ;; Delete if exists to prevent errors.
  ;; (when (uiop:file-exists-p output)
  ;;   (delete-file output))
  (dolist (name names)
    (qob-info "Building  system ~A" name)
    (asdf:operate :build-op name)))

;;; End of lisp/core/build.lisp
