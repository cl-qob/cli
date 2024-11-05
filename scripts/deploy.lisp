;;;; scripts/deploy.lisp --- Gather all dynamic link libraries.

;;; Commentary
;;
;; NOTE: Deploy and gather all dynamic link libraries.
;;

;;; Code

(load "scripts/_prepare.lisp")

(qob-copy-lisp-dir)
(qob-delete-exec)

;; Deploy
(asdf:make "qob-cli/deploy" :compression nil)

;;; End of scripts/deploy.lisp
