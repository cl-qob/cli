(defsystem "qob-cli"
  :version "0.1.0"
  :author "Jen-Chieh Shen"
  :license "MIT"
  :description "CLI for building, running, testing, and managing your Common Lisp dependencies"
  :homepage "https://github.com/cl-qob/cli"
  :depends-on ("clingon")
  :serial t
  :components (;; Utils
               (:file "src/el-lib")
               (:file "src/packages")
               (:file "src/utils")
               ;; Commands
               (:file "cmds/clean/workspace")
               (:file "cmds/core/build")
               (:file "cmds/core/clean")
               (:file "cmds/core/dists")
               (:file "cmds/core/info")
               (:file "cmds/core/install")
               (:file "cmds/core/install-deps")
               (:file "cmds/core/install-dists")
               (:file "cmds/core/list")
               (:file "cmds/core/status")
               (:file "cmds/core/uninstall")
               (:file "cmds/qob")
               ;; Program
               (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "bin/qob"
  :entry-point "qob-cli:main")
