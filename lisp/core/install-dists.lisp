;;;; lisp/core/install-dists.lisp --- Install dists

;;; Commentary
;;
;; Command use to install dists,
;;
;;   $ qob install-dists [names..]
;;

;;; Code

(let ((names qob-args))
  (cond ((zerop (length names))
         (qob-help "core/install-dists"))
        (t
         (dolist (name names)
           (ql-dist:install-dist name :prompt nil)))))

;;; End of lisp/core/install-dists.lisp
