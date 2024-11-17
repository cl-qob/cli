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

(qob-load "shared")

(qob-start
 (let ((names (qob-args)))
   (cond ((zerop (length names))
          (qob-help "core/uninstall"))
         (t
          (qob-uninstall-systems names)))))

;;; End of lisp/core/uninstall.lisp
