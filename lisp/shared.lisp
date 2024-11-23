;;; lisp/shared.el --- Shared functions

;;; Commentary:
;;
;; Functions cannot be compiled in `_prepare.el' but can be
;; compiled later on.
;;

;;; Code:

(defun qob-install-systems (names)
  "Install systems by NAMES."
  (when names
    (let* ((silent-p (not (qob-reach-verbosity-p 'debug)))
           (total (length names))
           (count 1)
           (installed 0)
           (skipped 0))
      (qob-msg "Installing ~A system~A... " total (qob--sinr total "" "s"))
      (qob-msg "")
      (dolist (name names)
        (let* ((system (ql-dist:find-system name))
               (installed-system (ignore-errors (asdf:find-system name)))
               (version (or (and installed-system
                                 (asdf:component-version installed-system))
                            ;;(slot-value system 'ql-dist:version)
                            "0"))
               (install-p))
          (cond (installed-system
                 (qob-msg "  - [~A/~A] Skipping ~A (~A)... already installed ✗"
                          count total
                          (qob-ansi-green name)
                          (qob-ansi-yellow version))
                 (incf skipped))
                (t
                 (qob-with-progress
                  (qob-format "  - [~A/~A] Installing ~A (~A)... "
                              count total
                              (qob-ansi-green name)
                              (qob-ansi-yellow version))
                  (qob-with-verbosity
                   'debug
                   (setq install-p
                         (ignore-errors (ql:quickload name :silent silent-p))))
                  (if install-p "done ✓" "skipped ✗"))
                 (when install-p
                   (incf installed)))))
        (incf count))
      (qob-msg "")
      (qob-info "(Total of ~A system~A installed; ~A skipped)" installed
                (qob--sinr installed "" "s")
                skipped))))

(defun qob-uninstall-systems (names)
  "Uninstall systesm by NAMES."
  (when names
    (let* ((total (length names))
           (count 1)
           (installed 0)
           (skipped 0))
      (qob-msg "Uninstalling ~A system~A... " total (qob--sinr total "" "s"))
      (qob-msg "")
      (dolist (name names)
        (let* ((installed-system (ignore-errors (asdf:find-system name)))
               (version (or (and installed-system
                                 (asdf:component-version installed-system))
                            "0")))
          (cond ((null installed-system)
                 (qob-msg "  - [~A/~A] Skipping ~A (~A)... not installed ✗"
                          count total
                          (qob-ansi-green name)
                          (qob-ansi-yellow version))
                 (incf skipped))
                (t
                 (qob-with-progress
                  (qob-format "  - [~A/~A] Uninstalling ~A (~A)... "
                              count total
                              (qob-ansi-green name)
                              (qob-ansi-yellow version))
                  (qob-with-verbosity 'debug (ql:uninstall name))
                  "done ✓")
                 (incf installed))))
        (incf count))
      (qob-msg "")
      (qob-info "(Total of ~A system~A uninstalled; ~A skipped)" installed
                (qob--sinr installed "" "s") skipped))))

;;; End of lisp/shared.lisp
