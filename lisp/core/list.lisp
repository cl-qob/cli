;;;; lisp/core/list.lisp --- Build executable

;;; Commentary
;;
;; Command use to list the registered system
;;
;;   $ qob list
;;

;;; Code

(qob-start
 (qob-setup)
 (format t "~A" (asdf/system-registry:registered-systems)))
