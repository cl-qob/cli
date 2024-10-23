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
    (qob-msg "Installing 2 systems... ")
    (qob-msg "")
    (dolist (name names)
      (let* ((system (asdf:find-system name))
             (version (asdf:component-version system))
             (already-installed-p (asdf:find-system name)))
        (if already-installed-p (incf skipped) (incf installed))
        (qob-with-progress
         (qob-format "  - [~A/~A] Installing ~A (~A)... "
                     count total
                     (qob-ansi-green name)
                     (qob-ansi-yellow version))
         (qob-silent (ql:quickload name))
         (if already-installed-p "skipped ✗" "done ✓")))
      (incf count))
    (qob-msg "")
    (qob-info "(Total of ~A systems installed; ~A skipped)" installed skipped)))

;;; End of lisp/shared.lisp
