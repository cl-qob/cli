;;; lisp/_el_lib.el --- Emacs Lisp Layer
;;; Commentary:
;;; Code:

;;
;;; Includes

(require "uiop")

;;
;;; Core

(defun qob-el-format (string &rest objects)
  "Mimic `format' function."
  (apply #'format nil string objects))

(defun qob-el-2str (object)
  "Convert to string."
  (cond ((stringp   object) object)
        ((pathnamep object) (namestring object))
        (t                  (format nil "~A" object))))

(defun qob-el-memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun qob-el-member (elt list)
  "Mimic `member' function."
  (member elt list :test #'string=))

(defun qob-el-file-name-directory (filename)
  "Return the directory component in file name FILENAME."
  (setq filename (qob-el-2str filename))
  (let ((dir (directory-namestring filename))
        (dirve (char filename 0)))
    (if (uiop:os-windows-p)
        (concatenate 'string (string dirve) ":" dir)
        dir)))

(defun qob-el-expand-fn (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function but return path object instead."
  (uiop:ensure-absolute-pathname (uiop:merge-pathnames* path dir-name)))

(defun qob-el-expand-file-name (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function; returns a string."
  (namestring (qob-el-expand-fn path dir-name)))

(defun qob-el-file-name-nondirectory (path)
  "Like `file-name-nondirectory' function."
  (setq path (qob-el-2str path))
  (let ((pathname (parse-namestring path)))
    (car (last (pathname-directory pathname)))))

;;; End of lisp/_el_lib.lisp
