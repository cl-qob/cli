;;;; core/build.lisp --- Build executable

;;; Commentary
;;
;; Command use to build the executable
;;
;;   $ qob build
;;
;;
;;  Optional arguments:
;;
;;    --name, -n         path to the ASD file
;;    --output, -o       output directory
;;

;;; Code

(qob-start
 (let* ((name   (clingon:getopt cmd :name))
        (output (clingon:getopt cmd :output)))
   ;; Delete if exists to prevent errors.
   (when (uiop:file-exists-p output)
     (delete-file output))
   ;; TODO: Change build path.
   (qob-setup)
   (asdf:operate :build-op name)))

;;; End of core/build.lisp
