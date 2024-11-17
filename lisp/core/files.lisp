;;;; lisp/core/files.lisp --- Print all system files

;;; Commentary
;;
;; Command use to print all system files,
;;
;;   $ qob files
;;

;;; Code

(defun qob-files--print-filename (filename)
  "Print out the FILENAME."
  (qob-println "~A" filename))

(qob-start
 (let* ((patterns (qob-args))  ; TODO: filter files.
        (files (qob-system-files)))
   (mapc #'qob-files--print-filename files)
   (qob-msg "")
   (if (zerop (length files))
       (qob-info "(No package files found)")
       (qob-info "(Total of ~A item~A listed)" (length files) (qob--sinr files "" "s")))))

;;; End of lisp/core/files.lisp
