---
title: 🔭 Finding Lisp
weight: 150
---

By default, packages are installed for the default SBCL, i.e., the one behind
the `sbcl` command. To pick a different lisp implementation, set the environment
variable `QOB_LISP` to the command name or executable path of the lisp implementation
to use:

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

Note that installed dependencies are scoped on the version of the lisp implementation.
So when switching between versions you will have to install the dependencies for each:

```sh
$ QOB_LISP="sbcl-2.2.9" qob install
```
