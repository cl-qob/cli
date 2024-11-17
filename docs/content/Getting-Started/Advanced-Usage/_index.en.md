---
title: 🔧 Advanced Usage
weight: 400
---

{{< toc >}}

`Qob` is just a regular Common Lisp file and should be read from Lisp
implementation itself! You can do:

```cl
; Regular Qob file content...

(setq qob-enable-color t)  ; Display color
```

# 🪝 Hooks

`qob` provides some hooks which enable you to execute code before and after
each command. The hooks look like so:

- `qpb-before-COMMAND-hook`
- `qob-after-COMMAND-hook`

For example, execute certain tasks after the command `qob build`:

```cl
(qob-add-hook 'qob-after-build-hook
              (lambda ()
                ;; Do tasks after build
                ))
```

Or hooks that run on every command:

- `qob-before-command-hook`
- `qob-after-command-hook`

```cl
(qob-add-hook 'qob-before-command-hook
              (lambda ()
                (format T "~A" (qob-command))))  ; print the current command
```

For subcommands that contain spaces, will concatenate with `/`:

```sh
$ qob clean workspace    # clean/workspace
$ qob create cl-project  # create/cl-project
```

therefore,

```cl
(qob-add-hook 'qob-before-clean/workspace-hook
              (lambda ()
                ;; do stuff before clean workspace...
                ))
```
