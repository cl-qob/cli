;;; lisp/_el_lib.el --- Emacs Lisp Layer
;;; Commentary:
;;; Code:

;;
;;; Includes

(require "uiop")

;;
;;; Interals

(defun qob-2str (object)
  "Convert to string."
  (cond ((stringp   object) object)
        ((pathnamep object) (namestring object))
        (t                  (format nil "~A" object))))

(defun qob--remove-last-char (str)
  "Removes the last character from STRING if it's not empty."
  (if (> (length str) 0)
      (subseq str 0 (1- (length str)))
      str))

(defun qob-listify (obj)
  "Turn OBJ to list."
  (if (listp obj) obj (list obj)))

(defun qob-s-replace (old new str)
  "Replaces OLD with NEW in S."
  (let ((pos (search old str)))
    (if pos
        (concatenate 'string
                     (subseq str 0 pos)
                     new
                     (subseq str (+ pos (length old))))
        str)))  ; Return original if substring not found

(defun qob-s-prefix-p (prefix string)
  "Checks if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun qob-s-suffix-p (suffix string)
  "Checks if STRING ends with SUFFIX."
  (and (<= (length suffix) (length string))
       (string= suffix (subseq string (- (length string) (length suffix))))))

(defun qob-s-remove-prefix (prefix string)
  "Removes PREFIX from STRING if STRING starts with PREFIX."
  (if (qob-s-prefix-p prefix string)
      (subseq string (length prefix))
      string))

(defun qob-s-remove-suffix (suffix string)
  "Removes SUFFIX from STRING if STRING ends with SUFFIX."
  (if (qob-s-suffix-p suffix string)
      (subseq string 0 (- (length string) (length suffix)))
      string))

(defun qob-s-slash-p (path)
  "Return t if end with slash."
  (qob-s-suffix-p "/" path))

(defun qob-s-slash (path)
  "Ensure path is a directory."
  (let ((path (qob-2str path)))
    (if (qob-s-slash-p path) path
        (concatenate 'string path "/"))))

(defun qob-f-root ()
  "Return root directory."
  (let* ((filename (uiop:getcwd))
         (filename (qob-2str filename))
         (drive (char filename 0)))
    (if (uiop:os-windows-p)
        (concatenate 'string (string drive) ":/")
        (string drive))))

;;
;;; Core

(defun qob-format (str &rest objects)
  "Mimic `format' function."
  (apply #'format nil str objects))

(defun qob-memq (elt list)
  "Mimic `memq' function."
  (member elt list :test #'eq))

(defun qob-member (elt list)
  "Mimic `member' function."
  (member elt list :test #'string=))

(defun qob-file-name-directory (filename)
  "Return the directory component in file name FILENAME."
  (setq filename (qob-2str filename))
  (when (qob-s-slash-p filename)
    (setq filename (qob--remove-last-char filename)))
  (let* ((dir (directory-namestring filename))
         (drive (char filename 0))
         (drive (string drive)))
    (if (uiop:os-windows-p)
        (concatenate 'string drive ":" dir (if (string= "" dir) "/" ""))
        dir)))

(defun qob-expand-fn (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function but return path object instead."
  (uiop:ensure-absolute-pathname (uiop:merge-pathnames* path dir-name)))

(defun qob-expand-file-name (path &optional (dir-name (uiop:getcwd)))
  "Like `expand-file-name' function; returns a string."
  (namestring (qob-expand-fn path dir-name)))

(defun qob-file-name-nondirectory (path)
  "Like `file-name-nondirectory' function."
  (setq path (qob-2str path))
  (let ((pathname (parse-namestring path)))
    (car (last (pathname-directory pathname)))))

(defun qob-locate-dominating-file (pattern &optional dir)
  "Find the file from DIR by PATTERN."
  (let* ((dir (or dir (uiop:getcwd)))
         (dir (qob-2str dir))
         (result))
    (uiop:with-current-directory (dir)
      (setq result (nth 0 (directory pattern)))
      (when (and (not result)
                 (not (string= (qob-f-root) dir)))
        (setq dir (qob-file-name-directory dir)  ; Up one dir.
              result (qob-locate-dominating-file pattern dir))))
    result))

;;; End of lisp/_el_lib.lisp
