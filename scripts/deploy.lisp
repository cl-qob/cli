;;;; scripts/deploy.lisp --- Gather all dynamic link libraries.

;;; Commentary
;;
;; NOTE: Deploy and gather all dynamic link libraries.
;;

;;; Code

(require 'asdf)

(load "~/quicklisp/setup.lisp")
(ql:quickload "deploy")

(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "qob-cli/deploy")

(load "scripts/_prepare.lisp")

(qob-delete-exec)

;; Deploy
(asdf:make "qob-cli/deploy" :compression nil)

;;; End of scripts/deploy.lisp
