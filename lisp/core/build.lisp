;;;; lisp/core/build.lisp --- Build executable

;;; Commentary
;;
;; Command use to build the executable
;;
;;   $ qob build
;;
;;
;;  Optional arguments:
;;
;;    --name, -n         path to the ASD file
;;

;;; Code

(qob-init-ql)
(qob-init-systems)

(defvar qob-build--count 0
  "Build count.")

(defvar qob-build--total -1
  "Total build count..")

(defun qob-build--print-header (total)
  "Print the build header."
  (setq qob-build--total total)
  (qob-println "Building ~A system~A... " total (qob--sinr total "" "s"))
  (qob-println ""))

(defun qob-build--system-by-name (name)
  "Build the system by system's NAME."
  (incf qob-build--count)
  (qob-with-progress
   (qob-format "  - [~A/~A] Building system ~A (~A)... "
               qob-build--count qob-build--total
               (qob-ansi-green name)
               (qob-ansi-yellow (qob-system-version name)))
   (qob-with-verbosity
    'debug
    (asdf:operate :build-op name))
   "done ✓"))

(let* ((systems qob-args)
       (systems-len (length systems))
       (default-name (qob-only-system)))
  (cond
    ;; If only specified one system.
    (default-name
     (qob-build--print-header 1)
     (qob-build--system-by-name default-name))
    ;; If no system(s) specified.
    ((zerop systems-len)
     (qob-help "core/build"))
    ;; Build system for all specify systems.
    (t
     (qob-build--print-header systems-len)
     (dolist (name systems)
       (qob-build--system-by-name name))))
  (when (<= 1 qob-build--total)
    (qob-msg "")
    (qob-info "(Total of ~A systems built)" qob-build--count)))

;;; End of lisp/core/build.lisp
