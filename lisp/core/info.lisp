;;;; lisp/core/info.lisp --- Print systems info

;;; Commentary
;;
;; Command use to print systems info,
;;
;;   $ qob info
;;

;;; Code

(qob-init-asds)

(defun qob-info--print-dep (deps)
  "Print DEPS."
  (dolist (dep deps)
    ;; TODO: Print system version?
    (qob-println "  ~A" dep)))

(defun qob-info--print-system (name)
  "Print the system's info by their NAME."
  (let* ((file        (qob-find-asd-file name))
         (system      (asdf:find-system name))
         (author      (asdf:system-author system))
         (maintainer  (asdf:system-maintainer system))
         (version     (asdf:component-version system))
         (description (asdf:system-description system))
         (homepage    (asdf:system-homepage system))
         (license     (asdf:system-license system))
         (depends-on  (asdf:system-depends-on system)))
    (qob-msg "")
    (qob-write "💡 ")
    (qob-info "Defined in file ~A" file)
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
      (qob-info--print-dep depends-on)
      (qob-info--print-dep (mapcar #'car qob-depends-on)))))

(qob-start
 (let ((names (qob-args))
       (primary-system (qob-primary-system-entry)))
   (cond
     ;; If specified system(s).
     (names
      (dolist (name names)
        (qob-info--print-system name)))
     ;; Print primary system.
     (primary-system
      (qob-info--print-system (car primary-system)))
     ;; Print help.
     (t
      (qob-help "core/info")))))

;;; End of lisp/core/info.lisp
