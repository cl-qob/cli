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
    (qob-println fmt
                 title
                 (qob-ansi-dark content)
                 note))
  (incf qob-status--listed))

(defun qob-status--list-max-length (lst index)
  "Return the LST max length by its INDEX."
  (let ((max-len 0)
        (max-current))
    (dolist (data lst)
      (setq max-current (qob-2str (nth index data))
            max-current (case index
                          (1 (qob-ansi-dark max-current))
                          (_ max-current))
            max-len (max (length max-current) max-len)))
    max-len))

(defun qob-status--print-infos (lst)
  "Print environment info LST."
  (let* ((len-0 (qob-2str (qob-status--list-max-length lst 0)))
         (len-1 (qob-2str (+ (qob-status--list-max-length lst 1) 2)))
         (fmt (concatenate 'string "   ~21A   ~" len-1 "A   ~A")))
    (dolist (pair lst)
      (when pair
        (qob-status--print-info fmt pair)))))

(defun qob-status--var-nil (var)
  "Return missing string when VAR is nil."
  (unless var
    (qob-ansi-red "(missing)")))

(defun qob-status--file-dir (path)
  "Return file directory status from PATH."
  (unless (uiop:probe-file* path)
    (qob-ansi-red "(missing)")))

;;
;;; Core

(qob-start
 (qob-println "In the ~A enviornment" (qob-status--environment-name))

 (qob-status--print-title "System:")
 (qob-status--print-infos
  `((,(lisp-implementation-type) ,(lisp-implementation-version))
    ("Machine instance" ,(machine-instance) ,(qob-status--var-nil (machine-instance)))
    ("Machine type" ,(machine-type) ,(qob-status--var-nil (machine-type)))
    ("Machine version" ,(machine-version) ,(qob-status--var-nil (machine-version)))
    ("Software type" ,(software-type) ,(qob-status--var-nil (software-type)))
    ("Software version" ,(software-version) ,(qob-status--var-nil (software-version)))))

 (qob-status--print-title "Environment:")
 (qob-status--print-infos
  `(("Qob directory" ,qob-dot ,(qob-status--file-dir qob-dot))
    ("Qob impls directory" ,(qob-dot-impls) ,(qob-status--file-dir (qob-dot-impls)))
    ("Quicklisp directory" ,(qob-ql-installed-dir) ,(qob-status--file-dir (qob-ql-installed-dir)))))

 (qob-status--print-title "Qob-file:")
 (qob-status--print-infos
  `(("Qob file" ,qob-file ,(qob-status--file-dir qob-file))))

 (when (qob-local-p)
   (qob-status--print-title "Workspace:")
   (qob-status--print-infos
    (mapcar (lambda (info)
              (setq info (append info
                                 ;; This can never happens since reading ASDF files
                                 ;; is the requirements of this command!
                                 (list (qob-status--file-dir (nth 1 info))))))
            qob-loaded-asds)))

 (qob-println "")
 (qob-info "(Total of ~A state~A listed)" qob-status--listed
           (qob--sinr qob-status--listed "" "s")))

;;; End of lisp/core/status.lisp
