(load "./src/main.lisp")

(setq uiop:*image-entry-point* #'qob:main)

(uiop:dump-image "./bin/qob.exe" :executable t)
