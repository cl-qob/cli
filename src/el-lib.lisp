;;;; src/el-lib.lisp --- Emacs Lisp library layer

;;; Commentary
;;
;; Emacs Lisp layer.
;;

;;; Code

(defpackage el-lib
  (:use cl)
  (:export el-memq
           el-member
           el-expand-fn
           el-executable-find
           el-move-path))

(in-package :el-lib)

(defvar el-executables nil
  "Executable cache.")

(defun el-executables ()
  "Return list of executables."
  (loop with path = (uiop:getenv "PATH")
        for p in (uiop:split-string path :separator
                                    (if (uiop:os-windows-p) ";" ":"))
        for dir = (probe-file p)
        when (uiop:directory-exists-p dir)
          append (uiop:directory-files dir)))

(defun el-executable-find (name)
  "Mimic `executable-find' function."
  (unless el-executables
    (setq el-executables (el-executables)))
  (find name el-executables
        :test #'equalp
        :key #'pathname-name))

(defun el-memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun el-member (elt list)
  (member elt list :test #'string=))

(defun el-expand-fn (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function."
  (uiop:ensure-absolute-pathname (uiop:merge-pathnames* path dir-name)))
