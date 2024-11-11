;;;; lisp/core/dists.lisp --- List out all dists

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
    (let ((name    (slot-value dist 'ql-dist:name))
          (version (slot-value dist 'ql-dist:version))
          (url     (or (qob-ignore-errors
                        (slot-value dist 'ql-dist::archive-base-url))
                       "n/a"))
          (enabled-p (ql-dist:enabledp dist)))
      (qob-println "   ~10A  ~15A  ~30A  ~A" name version url
                   (if enabled-p
                       (qob-ansi-green "(enabled)")
                       (qob-ansi-red "(disabled)"))))))

(let ((dists (ql-dist:all-dists)))
  (qob-info "Available dists:")
  (qob-msg "")
  (qob-dists--print dists)
  (qob-msg "")
  (qob-info "(Total of ~A dist~A available)" (length dists)
            (qob--sinr dists "" "s")))

;;; End of lisp/core/dists.lisp
