;;;; lisp/core/info.lisp --- Print systems info

;;; Commentary
;;
;; Command use to print systems info,
;;
;;   $ qob info
;;

;;; Code

(qob-init-asds)

(defun qob-info--print-system (name system)
  "Print the SYSTEM info."
  (let ((author      (asdf:system-author system))
        (maintainer  (asdf:system-maintainer system))
        (version     (asdf:component-version system))
        (description (asdf:system-description system))
        (homepage    (asdf:system-homepage system))
        (license     (asdf:system-license system))
        (depends-on  (asdf:system-depends-on system)))
    (qob-println "~A (~A) | deps: ~A"
                 (qob-ansi-green name)
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

(let ((names qob-args))
  (cond ((zerop (length names))
         (qob-help "core/info"))
        (t
         (dolist (name names)
           (let ((system (asdf:find-system name)))
             (qob-info--print-system name system))))))

;;; End of lisp/core/info.lisp
