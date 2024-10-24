;;;; lisp/core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(qob-init-ql)

(defun qob-list--print-system (name system)
  "Print the SYSTEM."
  (let ((version (or (asdf:component-version system)
                     "0"))
        (desc (or (asdf:system-description system)
                  "")))
    (qob-println "   [+] ~A  ~A  ~A"
                 name version desc)))

(defun qob-list--print-system-by-name (name)
  "Print the system info by NAME."
  (let ((system (asdf:find-system name)))
    (qob-list--print-system name system)))

(defun qob-list--print-dist (dist)
  ""
  (let ((systems (ql-dist:provided-systems dist)))
    (dolist (system systems)
      (let ((name    (ql-dist:name system))
            (version (or (ql-dist:version system)
                         "0"))
            )
        (qob-println "   [+] ~A (~A)"
                     name version)
        ))))

(let* ((pre-systems (asdf:registered-systems))
       (pre-systems (reverse pre-systems))
       (post-systems)
       (local-p (qob-local-p)))
  (when local-p
    (qob-init-systems))

  (setq post-systems (remove-if (lambda (system)
                                  (qob-el-memq system pre-systems))
                                (asdf:registered-systems))
        post-systems (reverse post-systems))

  (qob-println "Pre-built systems:")
  (qob-msg "")
  (mapc #'qob-list--print-system-by-name pre-systems)
  (qob-msg "")
  (qob-info "(Total of ~A system registered)" (length pre-systems)
            (qob--sinr pre-systems "" "s"))

  (when local-p
    (qob-msg "")
    (qob-println "User systems:")
    (qob-msg "")
    (mapc #'qob-list--print-system-by-name post-systems)
    (qob-msg "")
    (qob-info "(Total of ~A system~A registered)" (length post-systems)
              (qob--sinr post-systems "" "s")))

  (when (qob-all-p)
    (mapc #'qob-list--print-dist (ql-dist:all-dists))))

;;; End of lisp/core/list.lisp
