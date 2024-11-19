---
title: 🔰 快速開始
weight: 100
---

{{< toc >}}

使用 Qob 作為您的 Lisp 系統管理工具。

{{< hint info >}}
需要安裝 [Git](https://git-scm.com/downloads) 才能運行本教程。
{{< /hint >}}

## 🔍 步驟 1: 下載預先建立的檔案

請參閱[預建置檔案](https://cl-qob.github.io/Getting-Started/Install-Qob/#-prebuilt-binaries)部分。

{{< hint ok >}}
💡 如果遇到任何問題，請嘗試 [從原始碼建立](https://cl-qob.github.io/Getting-Started/Install-Qob/#-build-from-source)。
{{< /hint >}}

驗證您的新安裝：

```sh
$ qob --version
```

## 🔍 步驟 2: 導航到現有項目或創建新項目

如果您已有一個現有的 elisp 項目，請導航到項目根文件夾。

```sh
$ cd /path/to/project/dir/
```

創建一個：

```sh
$ qob create cl-project <your-project>
```

它應該在您當前的工作目錄中創建一個名為 `<your-project>` 的文件夾。

## 🔍 步驟 3： 創建 `Qob` 文件

接著創建項目中創建 Qob 文件：

```sh
$ qob init
```

您應該能夠在項目文件夾中看到一個 `Qob` 文件。 🎉🎊

## 🔍 步驟 4: 開始包開發

要檢查您的包裹信息，請運行：

```sh
$ qob info
```

您應該能夠看到以下信息：

```
your-package (1.0.0) | deps: 0
Your project description!
https://example.com/project-url/

Author: Your Name
License: MIT
```

從一開始，您就不會有任何 `dependencies`（默認為 `0`）！

## 🔍 步驟 5: 管理系統 dist

您可以使用 **Qob** 文件中的 `source` 指令來系統 dist。

```cl
(source "quicklisp")  ; 默認 dist
(source "ultralisp")  ; 新增 dist
```

{{< hint info >}}
💡 有關更多信息，請參閱 [DSL/source](https://cl-qob.github.io/DSL/#-source-alias)！
{{< /hint >}}

## 🔍 步驟 6: 添加一些依賴

現在您可以在 ASD 檔案中加入依賴。

```cl
(defsystem "your-project"
  ...
  :depends-on (flx str clingon)  ; 在這新增依賴!
  ...
```

您可以在 **Qob** 文件中使用 `depends-on` 指令添加當地依賴。

```cl
...

(depends-on "fsdb" "https://github.com/billstclair/fsdb" :git)
```

{{< hint danger >}}
💡 確認您新增的相依性在其中一個 dist 中可用！
{{< /hint >}}

## 🔍 步驟 7: 安裝依賴

現在我們可以安裝我們在 **ASD** 和 **Qob** 文件中指定的依賴項：

```sh
$ qob install-deps
```

您應該會看到 Qob 正確執行，輸出類似如下：

```
Loading ASDF files... done ✓
Installing 1 system...

  - [1/1] Installing fsdb from https://github.com/billstclair/fsdb... done ✓

(Total of 1 system installed; 0 skipped)
Installing 3 systems...

  - [1/3] Installing flx (0)... done ✓
  - [2/3] Installing str (0)... done ✓
  - [3/3] Installing clingon (0)... done ✓

(Total of 3 systems installed; 0 skipped)
```

## 🔗 也可以看看

- [Commands and options](https://cl-qob.github.io/Getting-Started/Commands-and-options/)
- [Domain Specific Language](https://cl-qob.github.io/DSL/)
- [Basic Usage](https://cl-qob.github.io/Getting-Started/Basic-Usage/)
