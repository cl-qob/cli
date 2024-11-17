---
title: 🔧 進階用法
weight: 400
---

{{< toc >}}

`Qob` 只是一個普通的 Common Lisp 文件，應該從 Lisp 實踐本身讀取！ 你可以做：

```cl
; 常規 Qob 文件內容...

(setq qob-enable-color t)  ; 顯示顏色
```

# 🪝 Hooks

`qob` 提供了一些 hooks，使您能夠在每個命令之前和之後執行代碼。 hook 看起來像這樣：

- `qob-before-COMMAND-hook`
- `qob-after-COMMAND-hook`

例如，在使用命令 `qob build` 進行建構時執行某些任務：

```cl
(qob-add-hook 'qob-after-build-hook
              (lambda ()
                ;; Do tasks after build
                ))
```

或者在每個命令上運行的 hooks：

- `qob-before-command-hook`
- `qob-after-command-hook`

```cl
(qob-add-hook 'qob-before-command-hook
              (lambda ()
                (format T "~A" (qob-command))))  ; print the current command
```

對於包含空格的子命令，將與`/`連接：

```sh
$ qob clean workspace    # clean/workspace
$ qob create cl-project  # create/cl-project
```

所以，

```cl
(qob-add-hook 'qob-before-clean/workspace-hook
              (lambda ()
                ;; 在 clean workspace 之前做一些事情...
                ))
```
