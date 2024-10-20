;;;; core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(qob-setup)

;; TODO: ..
(format t "~A" (asdf/system-registry:registered-systems))

;;; End of core/list.lisp
