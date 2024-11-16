---
title: 🚩 Commands and options
weight: 300
---

{{< toc >}}

The general syntax of the **qob** program is:

```sh
$ qob [GLOBAL-OPTIONS] [COMMAND] [COMMAND-OPTIONS] [COMMAND-ARGUMENTS]
```

# 🚩 Creating

## 🔍 qob create cl-project

Create a new Common Lisp project

```sh
$ qob [GLOBAL-OPTIONS] create cl-project <name>
```

{{< hint info >}}
💡 See https://github.com/fukamachi/cl-project for more information.
{{< /hint >}}

# 🚩 Core

Often use commands that are uncategorized.

## 🔍 qob init

Initialize the current directory to start using Qob.

```sh
$ qob [GLOBAL-OPTIONS] init
```

Qob will generate the file like this:

```cl
;; -*- mode: lisp; lexical-binding: t -*-

(source "quicklisp")
```

## 🔍 qob info

Show information about the project or configuration.

```sh
$ qob [GLOBAL-OPTIONS] info
```

## 🔍 qob status

Display the state of the workspace.

```sh
$ qob [GLOBAL-OPTIONS] status
```

## 🔍 qob install-deps

To install all dependencies.

```sh
$ qob [GLOBAL-OPTIONS] install-deps
```

## 🔍 qob install

To install systems.

```sh
$ qob [GLOBAL-OPTIONS] install [SYSTEMS..]
```

Install systems by specifying arguments:

```sh
$ qob install clingon clack
```

Or else, it will install the system from the current development:

```sh
$ qob install
```

## 🔍 qob uninstall

To uninstall/delete systems.

```sh
$ qob [GLOBAL-OPTIONS] uninstall [SYSTEMS..]
```

Uninstall systems by specifying arguments:

```sh
$ qob uninstall cl-autorepo cl-project
```

Or else, it will uninstall the system from the current development:

```sh
$ qob uninstall
```

## 🔍 qob package

Build the system artifact.

```sh
$ qob package [DESTINATION]
```

If [DESTINATION] is not specified, it will generate to the `/dist` folder
by default.

## 🔍 qob files

Print all system files.

```sh
$ qob files [DESTINATION]
```

# 🚩 Execution

Commands allow you to execute on top of the Qob core.

Basically, this allows you to do anything you want!

## 🔍 qob load

Load Common Lisp files in order.

```sh
$ qob [GLOBAL-OPTIONS] load [FILES..]
```

## 🔍 qob exec

Execute the system command with the given arguments.

```sh
$ qob [GLOBAL-OPTIONS] exec [COMMAND] [ARGUMENTS ...]
```

## 🔍 qob eval

Evaluate `FORM` as a lisp form.

```sh
$ qob [GLOBAL-OPTIONS] eval [FORM]
```

# 🚩 Management

Commands that help you manage your package's dependencies.

## 🔍 qob dists

List out all installed dists.

```sh
$ qob [GLOBAL-OPTIONS] dists
```

## 🔍 qob search

Search systems from archives.

```sh
$ qob [GLOBAL-OPTIONS] search [QUEIRES..]
```

## 🔍 qob list

List systems.

```sh
$ qob [GLOBAL-OPTIONS] list [--depth]
```

# 🚩 Linking

Link between this system and a dependency on the local filesystem. A linked
dependency avoids the need to download a dependency from a remote archive. The
system linked to must either have a `Qob`-file or a `ASD`-file.

## 🔍 qob link add

Links the given *source* directory into the system directory of this project,
under the given *system* name.

```sh
$ qob [GLOBAL-OPTIONS] link add <NAME> <PATH>
```

## 🔍 qob link delete

Deletes the link for the given systems.

```sh
$ qob [GLOBAL-OPTIONS] link delete [NAMES..]
```

## 🔍 qob link list

List all links.

```sh
$ qob [GLOBAL-OPTIONS] link list
```

# 🚩 Cleaning

Delete various files produced during building.

## 🔍 qob clean workspace

Delete `.qob` from the current workspace.

Alias: `.qob`

```sh
$ qob [GLOBAL-OPTIONS] clean workspace
```

# 🚩 Utilities

Other helper commands.

## 🔍 qob locate

Show Qob installed location.

```sh
$ qob [GLOBAL-OPTIONS] locate
```

# 🚩 Global Options

The following options are available on all Qob commands:

## 🔍 --global, -g

This will use `~/.qob/` instead of the package development environment.

This is used for other tasks. e.g., `cat`, etc.

```sh
$ qob -g [COMMAND]
```

## 🔍 --all, -a

Enable the `all` flag.

```sh
$ qob -a [COMMAND]
```

## 🔍 --force, -f

Force command's execution.

Force to uninstall the package `dash` even it's a dependency from another packages.

```sh
$ qob -f [COMMAND]
```

## 🔍 --allow-error

Continue the execution without killing the program.

## 🔍 --timestamps

Enable/Disable timestamps.

## 🔍 --log-level

Enable/Disable log header.

## 🔍 --log-file, --lf

Weather to generate log files.

## 🔍 --no-color

Disable color output.

## 🔍 --elapsed-time, --et

Show elapsed time between each operation.

## 🔍 --verbose, -v `<integer>`

Set verbosity from 0 to 5.

```sh
$ qob --verbose 4 [COMMAND]
```

## 🔍 --version

Show version number.

## 🔍 --help

Show help.


<!-- Links -->

[CircleCI]: https://circleci.com/
[GitHub Actions]: https://github.com/features/actions
[GitLab Runner]: https://docs.gitlab.com/runner/
[Travis CI]: https://www.travis-ci.com/
