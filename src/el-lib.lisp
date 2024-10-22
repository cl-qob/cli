;;;; src/el-lib.lisp --- Emacs Lisp library layer

;;; Commentary
;;
;; Emacs Lisp layer.
;;

;;; Code

(defpackage el-lib
  (:use cl)
  (:export el-2str
           el-2pathname
           el-memq
           el-member
           el-expand-fn
           el-expand-file-name
           el-executable-find
           el-move-path
           el-delete-directory
           el-directory-files
           el-file-name-directory))

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

(defun el-2str (object)
  "Convert to string."
  (cond ((stringp   object) object)
        ((pathnamep object) (namestring object))
        (t                  (format nil "~A" object))))

(defun el-2pathname (str)
  "Convert STR to pathname."
  (if (pathnamep str) str (make-pathname :directory str)))

(defun el-memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun el-member (elt list)
  "Mimic `member' function."
  (member elt list :test #'string=))

(defun el-expand-fn (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function but return path object instead."
  (uiop:ensure-absolute-pathname (uiop:merge-pathnames* path dir-name)))

(defun el-expand-file-name (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function; returns a string."
  (namestring (el-expand-fn path dir-name)))

(defun el-delete-directory (dir)
  "Delete the DIR."
  (sb-ext:delete-directory (el-lib:el-expand-fn dir) :recursive t))

(defun el-directory-files (dir)
  "Return a list of names of files in DIR."
  (append (uiop:subdirectories dir)
          (directory-files dir)))

(defun el-file-name-directory (filename)
  "Return the directory component in file name FILENAME."
  (setq filename (el-2str filename))
  (let ((dir (directory-namestring filename))
        (dirve (char filename 0)))
    (if (uiop:os-windows-p)
        (concatenate 'string (string dirve) ":" dir)
        dir)))

;;; End of src/el-lib.lisp
