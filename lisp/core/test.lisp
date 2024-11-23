;;;; lisp/core/test.lisp --- Run system tests

;;; Commentary
;;
;; Command use to run system tests,
;;
;;   $ qob test [names..]
;;

;;; Code

(qob-start
 (let ((names (qob-args))
       (primary-test-system (qob-primary-test-system-entry)))
   (cond
     ;; If specified system(s).
     (names
      (dolist (name names)
        (asdf:test-system name)))
     ;; Print primary system.
     (primary-test-system
      (asdf:test-system (car primary-test-system)))
     ;; Print help.
     (t
      (qob-help "core/test")))))

;;; End of lisp/core/test.lisp
