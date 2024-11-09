---
title: 🔭 尋找 Lisp
weight: 150
---

預設情況下，套件是為預設的 SBCL 安裝的，也就是在 `sbcl` 指令後面的那個。若要選擇不同的 lisp 實作，
請將環境變數 `QOB_LISP` 設定為要使用的 lisp 實作的指令名稱或執行路徑：

**Steel Bank Common Lisp (SBCL):**

```sh
$ QOB_LISP="sbcl-2.2.9" qob command
```

**Clozure CL (CCL):**

```sh
$ QOB_LISP="ccl-1.12" qob command
```

**Embeddable Common Lisp (ECL):**

```sh
$ QOB_LISP="ecl-1.12" qob command
```

請注意，安裝的相依性是以 lisp 實作的版本為範圍。因此，在不同版本之間切換時，您必須為每個版本安裝相依性：

```sh
$ QOB_LISP="sbcl-2.2.9" qob install
```
