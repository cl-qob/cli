;;;; lisp/core/install-deps.lisp --- Install dependent systems

;;; Commentary
;;
;; Command use to install dependent systems,
;;
;;   $ qob install-deps [names..]
;;

;;; Code

(qob-init-ql)
(qob-init-asds)

(qob-load "shared")

(cond ((zerop (length qob-args))
       (qob-help "core/install-deps"))
      (t
       (let ((systems qob-args))
         (dolist (system-name systems)
           (let* ((system (asdf:find-system system-name))
                  (deps   (asdf:system-depends-on system)))
             (qob-install-systems deps))))))

;;; End of lisp/core/install-deps.lisp
