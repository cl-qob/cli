---
title: 🔨 使用基礎
weight: 250
---

Qob 的 CLI 功能齊全但易於使用，即使對於那些使用命令行的經驗非常有限的人也是如此。

以下是您在開發 Qob 項目時將使用的最常用命令的說明。 請參閱
[命令和選項](https://cl-qob.github.io/Getting-Started/Commands-and-options/)
以全面了解 Qob 的 CLI。

一旦你安裝了 [Qob][]，確保它在你的 `PATH` 中。
您可以通過 help 命令測試 Qob 是否已正確安裝：

```
$ qob --help
```

您在控制台中看到的輸出應類似於以下內容：

```
NAME:
  qob - CLI for building, running, testing, and managing your Common Lisp dependencies

USAGE:
  qob [global-options] [<command>] [command-options] [arguments ...]

OPTIONS:
      --help           display usage information and exit
      --no-color       enable/disable color output
      --version        display version and exit
  -a, --all            enable all flag
  -g, --global         change default workspace to ~/.qob/
  -v, --verbose <INT>  set verbosity from 0 to 5 [default: 3]

COMMANDS:
  build          Build the executable
  clean          Delete various files produced during building
  create         Create a new Common Lisp project
  dists          List out all installed dists
  eval           Evaluate lisp form with a proper PATH
  files          Print all system files
  package        Build a system artifact
  info           Display information about the current system(s)
  init           Initialize project to use Qob
  install        Install systems
  install-deps   Automatically install system dependencies
  install-dists  Install dists
  list           List the registered systems
  load           Load lisp files
  locate         Print out Qob installed location
  status         Display the state of the workspace
  uninstall      Uninstall systems

AUTHORS:
  Jen-Chieh Shen <jcs090218@gmail.com>

LICENSE:
  MIT
```

## 🗃️ `qob` 命令

最常見的用法可能是在當前目錄作為輸入目錄的情況下運行 qob。
然後你運行 qpb 後跟一個子命令：

```sh
$ qob info             # 打印出 Qob 文件信息
```

Notice the subcommand can be nested:

```sh
$ qpb clean workspace  # 刪除你的 .qob 文件夾
```

傳遞選項 `--help` 以查找有關您正在使用的命令的更多信息：

```sh
$ qob clean --help
```

輸出，它顯示支持 2 個子命令：

```
NAME:
  qob clean - Delete various files produced during building

USAGE:
  qob clean <type>

OPTIONS:
      --help           display usage information and exit
      --no-color       enable/disable color output
      --version        display version and exit
  -a, --all            enable all flag
  -g, --global         change default workspace to ~/.qob/
  -v, --verbose <INT>  set verbosity from 0 to 5 [default: 3]

COMMANDS:
  all              Do all cleaning tasks
  dist             Delete dist subdirectory
  workspace, .qob  Clean up .qob directory
```

以下是已知的嵌套子命令列表：

- qob create
- qob clean

## 📌 了解你的 `quicklisp` 目錄

Qob 會建立一個隔離的環境，因此在播放、測試和執行您的 lisp 系統後，
它不會產生任何副作用。但重要的是，要知道目前的 Qob session 指向哪個
quicklisp 目錄 (通常與 `~/quicklisp/` 相同)，這樣才能釋放這個工具的全部潛力！

以下是 Qob 在不同場景下的幕後工作方式：

| 名稱   | 描述                                 | 選項               | 路徑      |
|--------|--------------------------------------|--------------------|-----------|
| local  | 默認行為，使用 Qob 作為系統開發工具  | n/a                | `./.qob/` |
| global | Qob 作為通用工具使用，與其他範圍無關 | `-g` or `--global` | `~/`      |

根據預設，Qob 會在**本地**範圍內安裝系統，將您的開發環境與全局系統隔離。
這允許您使用釘住的版本建立 Common Lisp 應用程式，而不會污染您的全局系統。

如果指定了 `-g` 或 `--global` 選項，您就可以像管理本地系統一樣管理您的全局系統。


<!-- Links -->

[Qob]: https://github.com/cl-qob/cli
