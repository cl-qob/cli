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
   (asdf:make name
              :compression (qob-compression-p))
   "done ✓"))

(defun qob-build--print-footer ()
  "Print the build footer."
  (qob-println "")
  (qob-info "(Total of ~A system~A built)" qob-build--count
            (qob--sinr qob-build--count "" "s")))

(let* ((systems (qob-args))
       (systems-len (length systems))
       (default-name (qob-only-system)))
  (cond
    ;; If only specified one system.
    (default-name
     (qob-build--print-header 1)
     (qob-build--system-by-name (car default-name))
     (qob-build--print-footer))
    ;; If no system(s) specified.
    ((zerop systems-len)
     (qob-help "core/build"))
    ;; Build system for all specify systems.
    (t
     (qob-build--print-header systems-len)
     (dolist (name systems)
       (qob-build--system-by-name name))
     (qob-build--print-footer))))

;;; End of lisp/core/build.lisp
