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

(qob-init-ql)

(let ((names qob-args))
  (cond ((zerop (length names))
         (qob-help "core/install"))
        (t
         (dolist (name names)
           (qob-info "Installing package ~A..." name)
           (ql:quickload name)))))

;;; End of lisp/core/install.lisp
