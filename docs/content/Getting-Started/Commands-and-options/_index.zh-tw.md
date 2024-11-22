---
title: 🚩 命令和選項
weight: 300
---

{{< toc >}}

**qob** 程式的一般語法為

```sh
$ qob [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS] [命令
```

# 🚩 創建

## 🔍 qob create cl-project

建立新的 Common Lisp 專案

```sh
$ qob [GLOBAL-OPTIONS] create cl-project <name
```

{{< hint info >}}
💡 更多資訊請參閱 https://github.com/fukamachi/cl-project。
{{< /hint >}}

# 🚩 核心

經常使用未分類的指令。

## 🔍 qob init

初始化目前目錄以開始使用 Qob。

```sh
$ qob [GLOBAL-OPTIONS] init
```

Qob 會產生這樣的檔案

```cl
;; -*- mode: lisp; lexical-binding: t -*-

(source 「quicklisp」)
```

## 🔍 qob info

顯示專案或組態的相關資訊。

```sh
$ qob [GLOBAL-OPTIONS] info
```

## 🔍 qob status

顯示工作區的狀態。

```sh
$ qob [GLOBAL-OPTIONS] status
```

## 🔍 qob install-deps

安裝所有的依賴項目。

```sh
$ qob [GLOBAL-OPTIONS] install-deps
```

## 🔍 qob install

要安裝系統

```sh
$ qob [GLOBAL-OPTIONS] install [SYSTEMS..］
```

透過指定參數安裝系統：

```sh
$ qob install clingon clack
```

否則，它會從目前的開發中安裝系統：

```sh
$ qob install
```

## 🔍 qob uninstall

要解除安裝/刪除系統。

```sh
$ qob [GLOBAL-OPTIONS] uninstall [SYSTEMS.］
```

透過指定參數來解除安裝系統：

```sh
$ qob uninstall cl-autorepo cl-project
```

否則會從目前的開發中卸載系統：

```sh
$ qob uninstall
```

## 🔍 qob package

建立系統工件。

```sh
$ qob package [DESTINATION] (目的地)
```

如果沒有指定 [DESTINATION]，預設會產生到 `/dist` 資料夾。

## 🔍 qob files

列印所有系統檔案。

```sh
$ qob files [DESTINATION]
```

# 🚩 執行

指令允許您在 Qob 核心之上執行。

基本上，這允許您做任何您想做的事！

## 🔍 qob load

依序載入 Common Lisp 檔案。

```sh
$ qob [GLOBAL-OPTIONS] load [FILES...］
```

## 🔍 qob exec

使用給定的參數執行系統指令。

```sh
$ qob [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]。
```

## 🔍 qob eval

評估 `FORM` 為 lisp 表格。

```sh
$ qob [GLOBAL-OPTIONS] eval [FORM] 。
```

# 🚩 管理

可協助您管理套件相依性的指令。

## 🔍 qob dists

列出所有已安裝的 dists。

```sh
$ qob [GLOBAL-OPTIONS] dists
```

## 🔍 qob search

從歸檔中搜尋系統。

```sh
$ qob [GLOBAL-OPTIONS] search [QUEIRES.］
```

## 🔍 qob list

列出系統。

```sh
$ qob [GLOBAL-OPTIONS] list [--depth] 列出系統。
```

# 🚩 連結

在本系統與本機檔案系統上的依賴之間建立連結。
連結的相依性可以避免從遠端存檔下載相依性。
連結的系統必須有 `Qob` 檔案 或 `ASD` 檔案。

## 🔍 qob link add

將指定的 *source* 目錄連結到本專案的系統目錄、
在指定的 *system* 名下。

```sh
$ qob [GLOBAL-OPTIONS] link add <NAME> <PATH
```

## 🔍 qob link delete

刪除指定系統的連結。

```sh
$ qob [GLOBAL-OPTIONS] link delete [NAMES.］
```

## 🔍 qob link list

列出所有連結。

```sh
$ qob [GLOBAL-OPTIONS] link list
```

# 🚩 清潔

刪除建立過程中產生的各種檔案。

## 🔍 qob clean workspace

從目前的工作區刪除 `.qob`。

別名: `.qob`

```sh
$ qob [GLOBAL-OPTIONS] clean workspace
```

## 🔍 qob clean dist

刪除 dist 子目錄。

```sh
$ qob [GLOBAL-OPTIONS] clean dist
```

## 🔍 qob clean all

此指令是所有其他清除指令的組合。

- `clean workspace`
- `clean dist`

```sh
$ qob [GLOBAL-OPTIONS] clean all
```

# 🚩 實用工具

其他輔助指令。

## 🔍 qob locate

顯示 Qob 安裝位置。

```sh
$ qob [GLOBAL-OPTIONS] locate
```

# 🚩 全域選項

下列選項適用於所有 Qob 指令：

## 🔍 --global, -g

這將使用 `~/.qob/` 而非套件開發環境。

```sh
$ qob -g [COMMAND]
```

## 🔍 --all, -a

啟用 `all` 旗標。

```sh
$ qob -a [COMMAND]
```

## 🔍 --allow-error

繼續執行而不終止程式。

## 🔍 --timestamps

啟用/停用時間戳記。

## 🔍 --log-level

啟用/停用記錄標頭。

## 🔍 --log-file, --lf

是否生成日誌文件。

## 🔍 --no-color

禁用顏色輸出。

## 🔍 --elapsed-time, --et

顯示每個操作之間經過的時間。

## 🔍 --verbose, -v `<integer>`

將詳細程度從 0 設置為 5。

```sh
$ qob --verbose 4 [COMMAND]
```

## 🔍 --version

顯示版本號。

## 🔍 --help

顯示幫助。


<!-- Links -->

[CircleCI]: https://circleci.com/
[GitHub Actions]: https://github.com/features/actions
[GitLab Runner]: https://docs.gitlab.com/runner/
[Travis CI]: https://www.travis-ci.com/
