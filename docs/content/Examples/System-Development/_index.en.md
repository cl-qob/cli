---
title: 📦 System Development
weight: 200
---

`Qob` is the magic file that `qob` will read it as the init file in Lisp implementation.

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
