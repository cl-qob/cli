;;;; lisp/core/init.lisp --- Initialize project to use Qob

;;; Commentary
;;
;; Command use to initialize the project to use Qob,
;;
;;   $ qob init
;;

;;; Code

(qob-start
 (when (directory "Qob")
   (qob-info "Qob-file already exist")
   (quit))

 (let* ((cwd (qob-2str (uiop:getcwd)))
        (filename (concatenate 'string cwd "Qob")))
   (qob-with-progress
    "✅ Initializing Qob file... "
    (with-open-file (str filename
                         :direction :output
                         :if-does-not-exist :create)
      ;; Write empty string.
      (format str ";; -*- mode: lisp; lexical-binding: t -*-

(source \"quicklisp\")
"))
    "done!")

   (qob-println "")
   (qob-println "Your new Qob file is created in ~A" filename)
   (qob-println "")
   (qob-println "  💡 You can view the file content with the `cat` command")
   (qob-println "")
   (qob-print "Visit ~A for quickstart guide and full documentation."
              qob-homepage)))

;;; End of lisp/core/init.lisp
