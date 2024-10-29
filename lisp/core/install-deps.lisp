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
(qob-init-file)

(qob-load "shared")

;;
;;; Local Projects

(ql:quickload "cl-autorepo")

(when (qob-init-file-p)
  (let ((cl-autorepo::*repo-dir* (qob-ql-local-dir))
        (total (length qob-local-systems))
        (count 1)
        (installed 0)
        (skipped 0))
    (qob-msg "Installing ~A system~A... " total (qob--sinr total "" "s"))
    (qob-msg "")
    (dolist (args qob-local-systems)
      (let* ((name (nth 0 args))
             (url (nth 1 args))
             (installed-p (qob-ignore-errors (asdf:find-system name))))
        (if installed-p (incf skipped) (incf installed))
        (qob-with-progress
         (qob-format "  - [~A/~A] Installing ~A from ~A... "
                     count total
                     (qob-ansi-green name)
                     (qob-ansi-cyan url))
         (qob-with-verbosity 'debug
                             (apply #'cl-autorepo:add-system args))
         (if installed-p "skipped ✗" "done ✓")))
      (incf count))
    (qob-msg "")
    (qob-info "(Total of ~A system~A installed; ~A skipped)" installed
              (qob--sinr installed "" "s")
              skipped)))

;;
;;; From dists

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
     (qob-install-deps--by-system-name (car default-name)))
    ;; If no system(s) specified.
    ((zerop (length systems))
     (qob-help "core/install-deps"))
    ;; Install depedencies for all specify systems.
    (t
     (dolist (system-name systems)
       (qob-install-deps--by-system-name system-name)))))

;;; End of lisp/core/install-deps.lisp
