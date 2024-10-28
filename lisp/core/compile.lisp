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
(qob-init)

(ql:quickload "cl-autorepo")

(let ((cl-autorepo::*repo-dir* (qob-ql-local-dir)))
  (dolist (args qob-local-systems)
    (apply #'cl-autorepo:add-system args)))

;;; End of lisp/core/compile.lisp
