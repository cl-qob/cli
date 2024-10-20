;;; _ql.el --- Functions overwrite when Quicklisp is loaded
;;; Commentary:
;;; Code:

(defun qob-quicklisp-install (dir)
  "For `_no_ql.lisp'."
  (quicklisp-quickstart:install :path dir))

;;; End of _ql.lisp

