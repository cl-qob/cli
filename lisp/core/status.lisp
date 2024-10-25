;;;; lisp/core/status.lisp --- Print the current workspace status

;;; Commentary
;;
;; Command use to print the workspace statu,
;;
;;   $ qob status
;;

;;; Code

(qob-init-asds)

(defun qob-status--environment-name ()
  "Get the working environment name."
  (cond ((qob-global-p) "global (~~/)")
        (t              "development (./)")))

(defun qob-status--print-title (str)
  "Print title STR."
  (qob-println "")
  (qob-println "~A" (qob-ansi-underscore str)))

(defun qob-status--invocation ()
  "Return the invocation path."
  (cond
    ((find-package :sb-ext) (truename (first sb-ext:*posix-argv*)))
    ((find-package :ccl) (first (ccl:command-line-arguments)))
    ((find-package :ext) (first ext:*argv*))
    ((find-package :sys) (sys:executable-pathname))
    (t (qob-error "Unsupported Lisp implementation."))))

;;
;;; Core

(qob-println "In the ~A enviornment" (qob-status--environment-name))

(qob-status--print-title "System:")
(qob-println "   ~A: ~A" (lisp-implementation-type) (lisp-implementation-version))
(qob-println "   Invocation: ~A" (qob-status--invocation))

(when (qob-local-p)
  (qob-status--print-title "Workspace:")
  (dolist (name qob-loaded-asds)
    (qob-println "   ~A" name)))

;;; End of lisp/core/status.lisp
