;;;; lisp/core/status.lisp --- Print the current workspace status

;;; Commentary
;;
;; Command use to print the workspace statu,
;;
;;   $ qob status
;;

;;; Code

(qob-init-asds)

(defvar qob-status--listed 0
  "Count information listed.")

(defun qob-status--environment-name ()
  "Get the working environment name."
  (cond ((qob-global-p) "global (~~/)")
        (t              "development (./)")))

(defun qob-status--print-title (title)
  "Print the TITLE."
  (qob-println "")
  (qob-println "~A" (qob-ansi-underscore title))
  (qob-println ""))

(defun qob-status--print-info (fmt pair)
  "Print environment info with FMT and PAIR."
  (let ((title   (qob-2str (nth 0 pair)))
        (content (qob-2str (nth 1 pair)))
        (note    (qob-2str (or (nth 2 pair) ""))))
    (qob-println (concatenate 'string "   " fmt)
                 title
                 (qob-ansi-dark content)
                 note))
  (incf qob-status--listed))

;;
;;; Core

(qob-println "In the ~A enviornment" (qob-status--environment-name))

(qob-status--print-title "System:")
(qob-status--print-info
 "~A version   ~A" `(,(lisp-implementation-type) ,(lisp-implementation-version)))
(qob-status--print-info "~A   ~A" `("Machine instance" ,(machine-instance)))
(qob-status--print-info "~A   ~A" `("Machine type" ,(machine-type)))
(qob-status--print-info "~A   ~A" `("Machine version" ,(machine-version)))
(qob-status--print-info "~A   ~A" `("Software type" ,(software-type)))
(qob-status--print-info "~A   ~A" `("Software version" ,(software-version)))

(qob-status--print-title "Environment:")
(qob-status--print-info "~A   ~A" `("Qob directory" ,qob-dot))
(qob-status--print-info "~A   ~A" `("Qob implementation directory" ,(qob-dot-impls)))
(qob-status--print-info "~A   ~A" `("Quicklisp directory" ,(qob-ql-installed-dir)))

(when (qob-local-p)
  (qob-status--print-title "Workspace:")
  (dolist (name qob-loaded-asds)
    (qob-status--print-info "~A ~A" `(,name "(system)"))))

(qob-println "")
(qob-info "(Total of ~A state~A listed)" qob-status--listed
          (qob--sinr qob-status--listed "" "s"))

;;; End of lisp/core/status.lisp
