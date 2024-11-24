---
title: 🔨 Basic Usage
weight: 250
---

Qob’s CLI is fully featured but simple to use, even for those who have very
limited experience working from the command line.

The following is a description of the most common commands you will use while
developing your Common Lisp project.
See the [Commands and options](https://cl-qob.github.io/Getting-Started/Commands-and-options/)
for a comprehensive view of Qob’s CLI.

Once you have installed [Qob][], make sure it is in your `PATH`. You can test
that Qob has been installed correctly via the help command:

```
$ qob --help
```

The output you see in your console should be similar to the following:

```
NAME:
  qob - CLI for building, running, testing, and managing your Common Lisp dependencies

USAGE:
  qob [global-options] [<command>] [command-options] [arguments ...]

OPTIONS:
      --help           display usage information and exit
      --no-color       enable/disable color output
      --version        display version and exit
  -a, --all            enable all flag
  -g, --global         change default workspace to ~/.qob/
  -v, --verbose <INT>  set verbosity from 0 to 5 [default: 3]

COMMANDS:
  build          Build the executable
  clean          Delete various files produced during building
  create         Create a new Common Lisp project
  dists          List out all installed dists
  eval           Evaluate lisp form with a proper PATH
  files          Print all system files
  package        Build a system artifact
  info           Display information about the current system(s)
  init           Initialize project to use Qob
  install        Install systems
  install-deps   Automatically install system dependencies
  install-dists  Install dists
  list           List the registered systems
  load           Load lisp files
  locate         Print out Qob installed location
  status         Display the state of the workspace
  uninstall      Uninstall systems

AUTHORS:
  Jen-Chieh Shen <jcs090218@gmail.com>

LICENSE:
  MIT
```

## 🗃️ The `qob` Command

The most common usage is probably to run qob with your current directory being
the input directory. Then you run qob followed by a subcommand:

```sh
$ qob info             # Print out Qob-file information
```

Notice the subcommand can be nested:

```sh
$ qob clean workspace  # Deletes your `.qob` folder
```

Pass in option `--help` to look up more information regarding the command you
are using:

```sh
$ qob clean --help
```

The output, and it shows there are 3 subcommands supported:

```
NAME:
  qob clean - Delete various files produced during building

USAGE:
  qob clean <type>

OPTIONS:
      --help           display usage information and exit
      --no-color       enable/disable color output
      --version        display version and exit
  -a, --all            enable all flag
  -g, --global         change default workspace to ~/.qob/
  -v, --verbose <INT>  set verbosity from 0 to 5 [default: 3]

COMMANDS:
  all              Do all cleaning tasks
  dist             Delete dist subdirectory
  workspace, .qob  Clean up .qob directory
```

Here is a list of known nested subcommands:

- qob create
- qob clean

## 📌 Knowing your `quicklisp` directory

Qob creates an isolated environment, so it won't create any side effects after
playing, testing, and running your lisp systems. But it's important to know
what quicklisp directory (normally it's the same to `~/quicklisp/`)
the current Qob session is pointing to, so you can release the full
potential of this tool!

Here is how Qob works behind the scene in different scenarios:

| Name   | Description                                               | Options            | Path      |
|--------|-----------------------------------------------------------|--------------------|-----------|
| local  | The default behavior, use Qob as system dev tool          | n/a                | `./.qob/` |
| global | Use Qob as a general tool, it's unrelated to other scopes | `-g` or `--global` | `~/`      |

By default, Qob installs systems in a **local** scope, isolating your
development environment from global systems. This allows you to build
Common Lisp applications with pinned versions without polluting your
global systems.

If the `-g` or `--global` option is specified, you can manage your global
systems just as you would manage local systems.


<!-- Links -->

[Qob]: https://github.com/cl-qob/cli
