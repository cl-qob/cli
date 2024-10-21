;;;; core/dists.lisp --- Build executable

;;; Commentary
;;
;; Command use to list all dists,
;;
;;   $ qob dists
;;

;;; Code

(qob-setup)

(defun qob-dists--print (dists)
  "Print list of dists."
  (dolist (dist dists)
    ;; TODO: Print useful information.
    (qob-println "~A" (ql-dist:archive-url dist))))

(let ((dists (ql-dist:all-dists)))
  (qob-info "Available dists:")
  (qob-msg "")
  (qob-dists--print dists)
  (qob-msg "")
  (qob-info "(Total of ~A dist~A available)" (length dists)
            (qob--sinr dists "" "s")))

;;; End of core/dists.lisp
