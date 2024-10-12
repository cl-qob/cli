#| scripts/build.lisp

Build the source to executable.

NOTE: This will soon be replace with this build tools!
|#

(push '*default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "qob")

;;(ql:quickload "cl-autorepo")
;;(ql:quickload "clingon")

;;(load "./src/build.lisp")
;;(load "./src/main.lisp")

;;(setq uiop:*image-entry-point* #'qob:main)

;;(uiop:dump-image "./bin/qob.exe" :executable t)

(asdf:operate :build-op "qob")
