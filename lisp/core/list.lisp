;;;; lisp/core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(qob-init-ql)

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

  (qob-info "Pre-built systems:")

  (dolist (system pre-systems)
    (qob-println "  ~A" system))

  (when local-p
    (qob-msg "")
    (qob-info "User systems:")

    (dolist (system post-systems)
      (qob-println "  ~A" system))))

;;; End of lisp/core/list.lisp
