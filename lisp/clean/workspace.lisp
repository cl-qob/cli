;;;; lisp/clean/workspace.lisp --- Clean up .qob directory

;;; Commentary
;;
;; Command use to clean up .qob directory
;;
;;   $ qob clean workspace
;;

;;; Code

(qob-start
 (let ((deleted))
   (qob-with-progress
    (qob-format "Deleting ~A... " qob-dot)
    (setq deleted (ignore-errors (delete-directory qob-dot :recursive t)))
    (if deleted "done ✓" "skipped ✗"))
   (qob-println "")
   (if deleted
       (qob-info "(Workspace is now cleaned)")
       (progn
         (qob-info "(Failed to clean up workspace)")
         (setq qob-no-cleaning-operation-p t)))))

;;; End of lisp/clean/workspace.lisp
