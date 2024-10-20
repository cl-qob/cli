;;; alexandria.el --- Alexandria Common Lisp utility library.
;;; Commentary:
;;; Code:

(defmacro qob-if-let (bindings &body (then-form &optional else-form))
  ".."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

;; (defmacro qob-if-let* (bindings &body (then-form &optional else-form))
;;   ".."
;;   (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
;;                            (list bindings)
;;                            bindings))
;;          (variables (mapcar #'car binding-list)))
;;     (labels ((bind (bindings then-form)
;;                (if bindings
;;                    `(let (,(car bindings))
;;                       (when ,(caar bindings)
;;                         ,(bind (cdr bindings) then-form)))
;;                    `(progn ,@then-form))))
;;       (bind binding-list then-form))))

(defmacro qob-when-let (bindings &body forms)
  ".."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro qob-when-let* (bindings &body body)
  ".."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))
