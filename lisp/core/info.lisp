;;;; lisp/core/info.lisp --- Print systems info

;;; Commentary
;;
;; Command use to print systems info,
;;
;;   $ qob info
;;

;;; Code

(qob-init-asds)

(defun qob-info--print-system (name)
  "Print the system's info by their NAME."
  (qob-println "")
  (let* ((system      (asdf:find-system name))
         (author      (asdf:system-author system))
         (maintainer  (asdf:system-maintainer system))
         (version     (asdf:component-version system))
         (description (asdf:system-description system))
         (homepage    (asdf:system-homepage system))
         (license     (asdf:system-license system))
         (depends-on  (asdf:system-depends-on system)))
    (qob-println "~A (~A) | deps: ~A"
                 (qob-ansi-green (string-downcase name))
                 (qob-ansi-yellow version)
                 (qob-ansi-cyan (length depends-on)))
    (when description
      (qob-println description))
    (when homepage
      (qob-println (qob-ansi-cyan homepage)))
    (when author
      (qob-println "")
      (qob-println "Author: ~A" (qob-ansi-white author)))
    (when maintainer
      (qob-println "Maintainer: ~A" (qob-ansi-white maintainer)))
    (when license
      (qob-println "License: ~A" (qob-ansi-white license)))
    (when depends-on
      (qob-println "")
      (qob-println "dependencies:")
      (dolist (dep depends-on)
        ;; TODO: Print system version?
        (qob-println "  ~A" dep)))))

(let ((names qob-args)
      (default-name (qob-only-system)))
  (cond
    ;; If only specified one system.
    (default-name
     (qob-info--print-system default-name))
    ;; If no system(s) specified.
    ((zerop (length names))
     (qob-help "core/info"))
    ;; Print all systems information.
    (t
     (dolist (name names)
       (qob-info--print-system name)))))

;;; End of lisp/core/info.lisp
