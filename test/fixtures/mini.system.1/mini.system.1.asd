(defsystem "mini.system.1"
  :version "0.1.0"
  :author "JenChieh"
  :mailto "jcs090218@gmail.com"
  :license "MIT"
  :homepage "https://github.com/cl-qob/cli"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Mini test system 1"
  :in-order-to ((test-op (test-op "mini.system.1/tests"))))

(defsystem "mini.system.1/tests"
  :author "JenChieh"
  :license "MIT"
  :depends-on ("mini.system.1"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mini.system.1"
  :perform (test-op (op c) (symbol-call :rove :run c)))
