;;;; lisp/core/test.lisp --- Run system tests

;;; Commentary
;;
;; Command use to run system tests,
;;
;;   $ qob test [names..]
;;

;;; Code

(defun qob-test--by-name (name)
  "Test the system's NAME."
  (let ((system (ignore-errors (asdf:find-system name))))
    (if system
        (asdf:test-system name)
        (qob-println "✗ The test system ~A not found, skipped" (qob-ansi-green name)))))

(qob-start
 (let ((names (qob-args))
       (primary-test-system (qob-primary-test-system-entry)))
   (cond
     ;; If specified system(s).
     (names
      (mapc #'qob-test--by-name names))
     ;; Print primary system.
     (primary-test-system
      (qob-test--by-name (car primary-test-system)))
     ;; Print help.
     (t
      (qob-help "core/test")))))

;;; End of lisp/core/test.lisp
