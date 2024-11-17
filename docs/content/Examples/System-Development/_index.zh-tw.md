---
title: 📦 System 開發
weight: 200
---

`Qob` 是魔法文件，`qob` 會將其讀取為 Lisp 實踐中的初始化文件。

```cl
;; -*- mode: lisp; lexical-binding: t -*-

(files "lisp"
       "scripts"
       "Makefile"
       "LICENSE"
       "Qob")

(source "quicklisp")
(source "ultralisp")
```
