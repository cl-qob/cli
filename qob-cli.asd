(defsystem "qob-cli"
  :version "0.2.0"
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
               (:file "cmds/create/cl-project")
               (:file "cmds/clean/all")
               (:file "cmds/clean/dist")
               (:file "cmds/clean/workspace")
               (:file "cmds/core/build")
               (:file "cmds/core/clean")
               (:file "cmds/core/create")
               (:file "cmds/core/dists")
               (:file "cmds/core/eval")
               (:file "cmds/core/files")
               (:file "cmds/core/package")
               (:file "cmds/core/info")
               (:file "cmds/core/init")
               (:file "cmds/core/install")
               (:file "cmds/core/install-deps")
               (:file "cmds/core/install-dists")
               (:file "cmds/core/list")
               (:file "cmds/core/load")
               (:file "cmds/core/locate")
               (:file "cmds/core/status")
               (:file "cmds/core/test")
               (:file "cmds/core/uninstall")
               (:file "cmds/qob")
               ;; Program
               (:file "src/main"))
  :build-operation "program-op"
  :build-pathname "bin/qob"
  :entry-point "qob-cli:main")
