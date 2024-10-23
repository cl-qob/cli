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
;;

;;; Code

(qob-init-ql)
(qob-init-systems)

(let ((names qob-args))
  (dolist (name names)
    (qob-with-progress
     (qob-format "Building system ~A... " (qob-ansi-green name))
     (asdf:operate :build-op name)
     "done ✓")))

;;; End of lisp/core/build.lisp
