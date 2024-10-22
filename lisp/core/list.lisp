;;;; lisp/core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(qob-init-ql)

(defun qob-list--print-system (name)
  "Print the system info by NAME."
  (let* ((system (asdf:find-system name))
         (version (or (asdf:component-version system)
                      "0")))
    (qob-println "   ~A (~A)"
                 (qob-ansi-green name)
                 (qob-ansi-yellow version))))

(let* ((pre-systems (asdf/system-registry:registered-systems))
       (pre-systems (reverse pre-systems))
       (post-systems)
       (local-p (qob-local-p)))
  (when local-p
    (qob-init-system))

  (setq post-systems (remove-if (lambda (system)
                                  (qob-el-memq system pre-systems))
                                (asdf/system-registry:registered-systems))
        post-systems (reverse post-systems))

  (qob-println "Pre-built systems:")
  (mapc #'qob-list--print-system pre-systems)

  (when local-p
    (qob-msg "")
    (qob-println "User systems:")
    (mapc #'qob-list--print-system post-systems)))

;;; End of lisp/core/list.lisp
