;;;; lisp/core/uninstall.lisp --- Uninstall systems

;;; Commentary
;;
;; Command use to uninstall systems,
;;
;;   $ qob uninstall [names..]
;;
;;
;;  Optional arguments:
;;
;;    [names..]     name of the system(s) to uninstall
;;

;;; Code

(qob-init-ql)

(qob-load "shared")

(let ((names qob-args))
  (cond ((zerop (length names))
         (qob-help "core/uninstall"))
        (t
         ;; TODO: ..
         ;;(qob-install-systems names)
         )))

;;; End of lisp/core/uninstall.lisp
