---
title: Domain Specific Language
weight: 200
---

This document provides a reference on the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language).

{{< toc >}}

# 🚩 Package contents

## 🔍 **files** (`&rest patterns`)

Specify list of files that are included in this project.

```cl
(files "foo.el")
(files "*.el" "core/*.el")
```

# 🚩 Tests

## 🔍 **script** (`name` `command` &rest `args`)

Add built-in scripts and their preset life cycle event as well as arbitrary
scripts.

```cl
(script "test" "echo This is a test!")
```

# 🚩 Dependencies

## 🔍 **source** (`alias`)

## 🔍 **source** (`name` `url`)

Add a system dist to install dependencies from.

```cl
(source "quicklisp")
(source "quicklisp" "http://beta.quicklisp.org/")
```

Available aliases:

- `quicklisp` (http://beta.quicklisp.org/)
- `ultralisp` (http://dist.ultralisp.org/)

## 🔍 **depends-on** (`&rest args`)

Specify a dependency for this system.

```elisp
(depends-on "fsdb" "https://github.com/billstclair/fsdb" :git)
```

{{< hint ok >}}
💡 Install dependencies with command **qob install-deps**!
{{< /hint >}}
