;;; lisp/shared.el --- Shared functions

;;; Commentary:
;;
;; Functions cannot be compiled in `_prepare.el' but can be
;; compiled later on.
;;

;;; Code:

(defun qob-install-systems (names)
  "Install systems by NAMES."
  (let* ((total (length names))
         (count 1)
         (installed 0)
         (skipped 0))
    (qob-msg "Installing ~A system~A... " total (qob--sinr total "" "s"))
    (qob-msg "")
    (dolist (name names)
      (let* ((system (ql-dist:find-system name))
             ;;(version (slot-value system 'ql-dist:version))
             (version "0")
             (already-installed-p (qob-ignore-errors (asdf:find-system name))))
        (if already-installed-p (incf skipped) (incf installed))
        (qob-with-progress
         (qob-format "  - [~A/~A] Installing ~A (~A)... "
                     count total
                     (qob-ansi-green name)
                     (qob-ansi-yellow version))
         (qob-with-verbosity 'debug (ql:quickload name))
         (if already-installed-p "skipped ✗" "done ✓")))
      (incf count))
    (qob-msg "")
    (qob-info "(Total of ~A systems installed; ~A skipped)" installed skipped)))

(defun qob-uninstall-systems (names)
  "Uninstall systesm by NAMES."
  (let* ((total (length names))
         (count 1)
         (installed 0)
         (skipped 0))
    (qob-msg "Uninstalling ~A system~A... " total (qob--sinr total "" "s"))
    (qob-msg "")
    (dolist (name names)
      (let* ((system (qob-ignore-errors (asdf:find-system name)))
             (version (if system (asdf:component-version system)
                          "0")))
        (if system (incf installed) (incf skipped))
        (qob-with-progress
         (qob-format "  - [~A/~A] Uninstalling ~A (~A)... "
                     count total
                     (qob-ansi-green name)
                     (qob-ansi-yellow version))
         (qob-with-verbosity 'debug (ql:uninstall name))
         (if system "done ✓" "skipped ✗")))
      (incf count))
    (qob-msg "")
    (qob-info "(Total of ~A systems uninstalled; ~A skipped)" installed skipped)))

;;; End of lisp/shared.lisp
