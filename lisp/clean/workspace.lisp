;;;; lisp/clean/workspace.lisp --- Clean up .qob directory

;;; Commentary
;;
;; Command use to clean up .qob directory
;;
;;   $ qob clean workspace
;;

;;; Code

(qob-start
 (qob-with-progress
  (qob-format "Deleting ~A... " qob-dot)
  (uiop:delete-directory-tree (pathname qob-dot) :validate t)
  "done ✓")

 (qob-println "")
 (qob-info "(Workspace is now cleaned)"))

;;; End of lisp/clean/workspace.lisp
