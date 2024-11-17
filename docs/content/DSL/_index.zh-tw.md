---
title: 领域特定语言
weight: 200
---

本文檔是關於 [DSL] (https://en.wikipedia.org/wiki/Domain-specific_language)。

{{< toc >}}

# 🚩 Package contents

## 🔍 **files** (`&rest patterns`)

指定包含在此專案中的檔案清單。

```cl
(files "foo.el")
(files "*.el" "core/*.el")
```

# 🚩 測試

## 🔍 **script** (`name` `command` &rest `args`)

新增內建指令碼及其預設生命週期事件，以及任意指令碼。

```cl
(script "test" "echo This is a test!")
```

# 🚩 依賴

## 🔍 **source** (`alias`)

## 🔍 **source** (`name` `url`)

新增一個系統 dist 來安裝相依性。

```cl
(source "quicklisp")
(source "quicklisp" "http://beta.quicklisp.org/")
```

可用別名：

- `quicklisp` (http://beta.quicklisp.org/)
- `ultralisp` (http://dist.ultralisp.org/)

## 🔍 **depends-on** (`&rest args`)

指定此系統的相依性。

```elisp
(depends-on "fsdb" "https://github.com/billstclair/fsdb" :git)
```

{{< hint ok >}}
💡 使用 **qob install-deps** 指令安裝相依性！
{{< /hint >}}
