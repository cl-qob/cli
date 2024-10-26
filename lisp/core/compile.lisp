;;;; lisp/core/compile.lisp --- Compile files

;;; Commentary
;;
;; Command use to compile files
;;
;;   $ qob compile
;;
;;
;;  Optional arguments:
;;
;;    --name, -n         path to the ASD file
;;

;;; Code

(qob-init-ql)

(ql:quickload "cl-autorepo")

(let ((*repo-dir* ))
  (dolist (args qob-local-systems)
    (apply #' cl-autorepo:add-system args)))

;;; End of lisp/core/compile.lisp
