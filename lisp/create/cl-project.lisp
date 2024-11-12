;;;; lisp/create/cl-project.lisp --- Clean up .qob directory

;;; Commentary
;;
;; Command use to create a new project with `cl-project'
;;
;;   $ qob create cl-project
;;

;;; Code

(ql:quickload "cl-project")

(defun qob-read-line (prompt &optional default-value)
  "Like `read-line'."
  (let* ((prompt (if default-value (qob-format "~A(~A) " prompt default-value)
                     prompt))
         (answer))
    (qob-print prompt)
    (setq answer (read-line))
    (if (and default-value (string= "" answer))
        default-value
        answer)))

(defun qob-create-cl-project--do (name-or-path)
  "Create project by NAME-OR-PATH."
  (qob-println "Initialize the .asd file for your project...")
  (qob-println "")
  (let* ((name-or-path (qob-2str name-or-path))
         (output (qob-expand-file-name (qob-s-slash name-or-path)))
         (dirname (qob-file-name-nondirectory output))
         (home (qob-s-slash (user-homedir-pathname)))
         (username (qob-file-name-nondirectory home)))
    (cl-project:make-project
     (pathname output)
     :name        (qob-read-line "Name: "        dirname)
     :version     (qob-read-line "Version: "     "1.0.0")
     :description (qob-read-line "Description: " nil)
     :author      (qob-read-line "Author: "      username)
     :email       (qob-read-line "Email: "       nil)
     :homepage    (qob-read-line "Homepage: "    nil)
     :license     (qob-read-line "License: "     "MIT"))
    (qob-println "")
    (qob-println "")
    (qob-println "Preparing your new Common Lisp proejct... done ✓")
    (qob-println "Congratulations! Your new Common Lisp project is created in ~A" output)
    (qob-println "")
    (qob-println "  [1] Navigate to ~A" output)
    (qob-println "  [2] Try out the command `qob info`")
    (qob-println "")
    (qob-print "Visit ~A for quickstart guide and full documentation."
               qob-homepage)))

(let ((name-or-path (car qob-args))
      (len (length qob-args)))
  (cond ((zerop len)
         (qob-error "Required exactly 1 positional argument"))
        ((= 1 len)
         (qob-create-cl-project--do name-or-path))
        (t (qob-help "create/cl-project"))))

;;; End of lisp/create/cl-project.lisp
