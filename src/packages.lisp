;;;; src/packages.lisp --- Package definitions

;;; Commentary
;;
;; Define package definitions.
;;

;;; Code

(defpackage qob
  (:use cl)
  (:export
   ;; src/utils.lsip
   call-script
   call-impls
   setup           ; TODO: move to prepare.lisp
   load-system     ; TODO: move to prepare.lisp
   asd-files       ; TODO: move to prepare.lisp
   asd-test-files  ; TODO: move to prepare.lisp
   ;; src/logger.lsip
   print           ; TODO: move to prepare.lisp
   trace           ; TODO: move to prepare.lisp
   debug           ; TODO: move to prepare.lisp
   info            ; TODO: move to prepare.lisp
   warning         ; TODO: move to prepare.lisp
   error           ; TODO: move to prepare.lisp
   ;; cmds/qob.lisp
   command
   ;; src/main.lsip
   main))

(in-package :qob)
