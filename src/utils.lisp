;;;; src/utils.lisp --- Utilities module

;;; Commentary
;;
;; Where we place random utilities functions.
;;

;;; Code

(in-package :qob)

(defvar *lisp-root* "lisp/"
  "Directory path points to the lisp folder.")

(defun program-name ()
  "Lisp program we target to run."
  (or (uiop:getenv "QOB_LISP")
      "sbcl"))

(defun lisp-script (name)
  "Form lisp script path."
  (concatenate 'string name ".lisp"))

(defun call-lisp (script &rest args)
  "Run the lisp implementation."
  (let ((lisp-impls (program-name)))
    (unless (el-lib:el-executable-find lisp-impls)
      (error "Defined Lisp implementation is not installed: ~A" lisp-impls))
    (let* ((lisp-dir (el-lib:el-expand-fn *lisp-root* sb-ext:*runtime-pathname*))
           (script (lisp-script script)))
      (format t "~A" (el-lib:el-expand-fn script lisp-dir))
      ;; (uiop:run-program (list lisp-impls
      ;;                         "--load" (el-lib:el-expand-fn script *lisp-root*)
      ;;                         )
      ;;                   :output t
      ;;                   :force-shell t)
      )))

(defun setup ()
  "Setup the system."
  (let ((files (asd-files t)))
    (mapc (lambda (file)
            (load-system file)
            (-info "Load ASD file ~A" file))
          files)))

(defun load-system (filename)
  "Load the system from ASD's FILENAME; and return the registered name."
  (let ((dir (uiop:pathname-parent-directory-pathname filename))
        (file (pathname-name filename)))
    (push dir asdf:*central-registry*)
    (asdf:load-system file)
    file))  ; registered name

(defun find-system (name)
  "Return a system of given NAME."
  (asdf/system-registry:registered-system name))

(defun asd-files (&optional with-test)
  "Return a list of ASD files.

If optional argument WITH-TEST is non-nil; include test ASD files as well."
  (uiop:if-let ((files (directory "*.asd"))
                (_ (not with-test))
                (tests (asd-test-files)))
    (remove-if (lambda (filename) (el-lib:el-memq filename tests)) files)
    files))

(defun asd-test-files ()
  "Return a list of ASD test files."
  (directory "*-test*.asd"))
