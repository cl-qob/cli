#| scripts/build.lisp

Build the source to executable.

NOTE: This will soon be replace with this build tools!
|#

(load "./src/main.lisp")

(setq uiop:*image-entry-point* #'qob:main)

(uiop:dump-image "./bin/qob.exe" :executable t)
