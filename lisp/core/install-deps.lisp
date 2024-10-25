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

(defun qob-install-deps--by-system-name (name)
  "Install dependencies by system's NAME."
  (let* ((system (asdf:find-system name))
         (deps   (asdf:system-depends-on system)))
    (qob-install-systems deps)))

(let ((systems qob-args)
      (default-name (qob-only-system)))
  (cond
    ;; If only specified one system.
    (default-name
     (qob-install-deps--by-system-name default-name))
    ;; If no system(s) specified.
    ((zerop (length systems))
     (qob-help "core/install-deps"))
    ;; Install depedencies for all specify systems.
    (t
     (dolist (system-name systems)
       (qob-install-deps--by-system-name system-name)))))

;;; End of lisp/core/install-deps.lisp
