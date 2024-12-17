;;;; lisp/core/install-deps.lisp --- Install dependent systems

;;; Commentary
;;
;; Command use to install dependent systems,
;;
;;   $ qob install-deps [names..]
;;

;;; Code

(qob-init-asds)

(qob-load "shared")

;;
;;; Local Projects

(ql:quickload "cl-autorepo" :silent t)

;; Load only when Qob file exists.
(when qob-depends-on
  (let ((cl-autorepo::*repo-dir* (qob-ql-local-dir))
        (total (length qob-depends-on))
        (count 1)
        (installed 0)
        (skipped 0))
    (qob-msg "Installing ~A system~A... " total (qob--sinr total "" "s"))
    (qob-msg "")
    (dolist (args qob-depends-on)
      (let* ((name (nth 0 args))
             (url (nth 1 args))
             (installed-p (ignore-errors (asdf:find-system name))))
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
  (when name
    (let* ((system (asdf:find-system name))
           (deps   (asdf:system-depends-on system)))
      (if deps (qob-install-systems deps)
          (qob-info "(The system `~A' has no dependencies specified, skipped)" name)))))

(qob-start
 (let ((systems (qob-args))
       (primary-system (qob-primary-system-entry))
       (primary-test-system (qob-primary-test-system-entry)))
   (cond
     ;; If specified system(s).
     (systems
      (dolist (system-name systems)
        (qob-install-deps--by-system-name system-name)))
     ;; Print primary system.
     (primary-system
      (qob-install-deps--by-system-name (car primary-system))
      (when (qob-dev-p)
        (qob-install-deps--by-system-name (car primary-test-system))))
     ;; Print help.
     (t
      (qob-help "core/install-deps")))))

;;; End of lisp/core/install-deps.lisp
