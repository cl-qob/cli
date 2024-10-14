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
           el-move-path
           el-copy-directory))

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

(defun el-directory-files (directory)
  "Return a list of names of files in DIRECTORY."
  (append (uiop:subdirectories directory)
          (directory-files directory)))

(defun el-subdirectory-p (to-pathname from-pathname)
  "Return non-nil if TO-PATHNAME is part of the subdirectory to FROM-PATHNAME."
  (let ((to-dir (pathname-directory to-pathname))
        (from-dir (pathname-directory from-pathname)))
    (assert (eq :absolute (car to-dir)))
    (assert (eq :absolute (car from-dir)))
    (and (<= (length from-dir)
             (length to-dir))
         (loop :for from-elt :in (cdr from-dir)
               :for to-elt :in (cdr to-dir)
               :when (not (equal from-elt to-elt))
                 :do (return nil)
               :finally (return t)))))

(defun el-list-directory (directory &key directory-only (sort-method :pathname))
  "TODO: .."
  (delete nil
          (mapcar (lambda (x) (and (virtual-probe-file x directory) x))
                  (append (sort-files-with-method
                           (copy-list (uiop:subdirectories directory))
                           :sort-method sort-method)
                          (unless directory-only
                            (sort-files-with-method (uiop:directory-files directory)
                                                    :sort-method sort-method))))))

(defun el-copy-directory (src dst)
  "Recursively copy the contents of SRC to DST."
  (setf dst (uiop:ensure-directory-pathname dst))
  (let* ((src (el-expand-fn src))
         (dst (el-expand-fn dst)))
    (when (el-subdirectory-p dst src)
      (error "Cannot copy `~A' into its subdirectory `~A'" src dst))
    (let ((dst (ensure-directories-exist dst)))
      (dolist (file (el-list-directory src))
        (copy-file file dst)))
    (when *rename-p* (uiop:delete-empty-directory src))
    ))
