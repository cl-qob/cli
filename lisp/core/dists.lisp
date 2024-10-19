;;;; lisp/core/dists.lisp --- Build executable

;;; Commentary
;;
;; Command use to list all dists,
;;
;;   $ qob dists
;;

;;; Code

(defun qob-dists--print (dists)
  "Print list of dists."
  (dolist (dist dists)
    (qob-println "~A" dist)))

(qob-start
 (let ((dists (ql-dist:all-dists)))
   (qob-info "Available dists:")
   (qob-msg "")
   (qob-dists--print dists)
   (qob-info "(Total of ~A dist~A available)" (length dists)
             (qob--sinr dists))))
