;;;; lisp/core/eval.lisp --- Evaluate lisp form with a proper PATH

;;; Commentary
;;
;; Command use to eval lisp form.
;;
;;   $ qob eval [forms..]
;;

;;; Code

(qob-start
 (let* ((name (qob-args 0)))
   (qob-info name)
   (if name
       (qob-eval name)
       (progn
         (qob-info "(No expression found)")
         (qob-help "core/eval")))))

;;; End of lisp/core/eval.lisp
