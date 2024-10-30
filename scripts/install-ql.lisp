;;;; scripts/install-ql.lisp --- Install Quicklisp

;;; Commentary
;;
;; Install Quicklisp.
;;

;;; Code

(quicklisp-quickstart:install)

(ql:add-to-init-file)

;;; End of scripts/install-ql.lisp
