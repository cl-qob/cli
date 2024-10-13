;;;; src/el-lib.lisp --- Emacs Lisp library layer

;;; Commentary
;;
;; Emacs Lisp layer.
;;

;;; Code

(defpackage el-lib
  (:use cl)
  (:export memq
           expand-fn))

(in-package :el-lib)

(defun memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun expand-fn (path-string &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function."
  (uiop:unix-namestring
   (uiop:ensure-absolute-pathname
    (uiop:merge-pathnames*
     (uiop:parse-unix-namestring path-string))
    dir-name)))
