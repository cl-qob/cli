;;;; lisp/core/load.lisp --- Load lisp files

;;; Commentary
;;
;; Command use to load lisp files
;;
;;   $ qob load [files..]
;;

;;; Code

(qob-start
 (let ((files (qob-expand-file-specs (qob-args))))
   (if files
       (mapc #'load files)
       (qob-info "(Nothing to load.)"))))

;;; End of lisp/core/load.lisp
