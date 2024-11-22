;;;; lisp/clean/all.lisp --- Do all cleaning tasks

;;; Commentary
;;
;; Command use to do all cleaning tasks
;;
;;   $ qob clean all
;;

;;; Code

(defconstant qob-clean-all--tasks-total 2
  "Count cleaning task.")

(defvar qob-clean-all--tasks-count 0
  "Count cleaning task.")

(defvar qob-clean-all--tasks-cleaned 0
  "Total cleaned tasks.")

(defmacro qob--clean-section (title &rest body)
  "Print clean up TITLE and execute BODY."
  `(let (qob-no-cleaning-operation-p)
     (incf qob-clean-all--tasks-count)
     (qob-with-progress
      (concatenate 'string
                   (qob-format "  - [~A/~a] " qob-clean-all--tasks-count qob-clean-all--tasks-total)
                   (qob-format "~A... " ,title))
      (qob-with-verbosity 'debug ,@body)
      (if qob-no-cleaning-operation-p
          "skipped ✗"
          (progn
            (incf qob-clean-all--tasks-cleaned)
            "done ✓")))))

(qob-start
 (qob-msg "Applying ~A cleaning tasks..." qob-clean-all--tasks-total)
 (qob-msg "")
 (qob--clean-section (qob-format "Cleaning ~A" (qob-ansi-green "workspace"))
                     (qob-call "clean/workspace"))
 (qob--clean-section (qob-format "Cleaning ~A" (qob-ansi-green "dist"))
                     (qob-call "clean/dist"))
 (qob-msg "")
 (qob-info "(Total of ~A task~A cleaned, ~A skipped)"
           qob-clean-all--tasks-cleaned
           (qob--sinr qob-clean-all--tasks-cleaned "" "s")
           (- qob-clean-all--tasks-count qob-clean-all--tasks-cleaned)))

;;; End of lisp/clean/all.lisp
