;;;; lisp/core/install.lisp --- Install systems

;;; Commentary
;;
;; Command use to install systems,
;;
;;   $ qob install [names..]
;;
;;
;;  Optional arguments:
;;
;;    [names..]     name of the system(s) to install
;;

;;; Code

(qob-load "shared")

(qob-start
 (let ((names (qob-args)))
   (cond ((zerop (length names))
          (qob-help "core/install"))
         (t
          (qob-install-systems names)))))

;;; End of lisp/core/install.lisp
