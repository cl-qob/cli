;;;; lisp/core/install-deps.lisp --- Install dependent systems

;;; Commentary
;;
;; Command use to install dependent systems,
;;
;;   $ qob install-deps
;;

;;; Code

(qob-init-ql)
(qob-init-asds)

(dolist (asd qob-loaded-asds)
  (qob-println "ASD: ~A" (asdf:component-depends-on asd nil))
  )

;;; End of lisp/core/install-deps.lisp
