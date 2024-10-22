;;;; lisp/core/install.lisp --- Build executable

;;; Commentary
;;
;; Command use to install packages,
;;
;;   $ qob install [names..]
;;
;;
;;  Optional arguments:
;;
;;    [names..]     name of the package(s) to install
;;

;;; Code

(qob-init-ql)

(dolist (name qob-args)
  (qob-info "Installing package ~A..." name)
  (ql:quickload name))

;;; End of lisp/core/install.lisp
