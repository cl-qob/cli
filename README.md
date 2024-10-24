<a href="#"><img align="right" src="./docs/static/logo.png" width="20%"></a>

# Qob
> CLI for building, running, testing, and managing your Common Lisp dependencies

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![SBCL Version](https://img.shields.io/badge/SBCL-2.2.9+-white.svg?logo=awslambda&logoColor=white)](https://www.gnu.org/software/emacs/download.html)
[![Release](https://img.shields.io/github/tag/cl-qob/cli.svg?label=release&logo=github)](https://github.com/cl-qob/cli/releases/latest)
[![CI](https://github.com/cl-qob/cli/actions/workflows/test.yml/badge.svg)](https://github.com/cl-qob/cli/actions/workflows/test.yml)

Qob attempts to be good at these things:

- Play nicely with [ASDF][] and [Quicklisp][].
- Support project local like [Qlot][] and support global scope facilities.
- Works on all modern operating systems: [Linux][], [macOS][], and [Windows][].

We aim to make Qob an all-in-one tool so users no longer need to install tools like [Roswell] and [Qlot][], which only serve specific goals.

## 🔧 Usage

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
  dists          List out all installed dists
  info           Display information about the current system(s)
  install        Install systems
  install-deps   Automatically install system dependencies
  install-dists  Install dists
  list           List the registered system
  status         Display the state of the workspace
  uninstall      Uninstall systems

AUTHORS:
  Jen-Chieh Shen <jcs090218@gmail.com>

LICENSE:
  MIT
```

## 🔨 Development

WIP

## ⚜️ License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

See [`LICENSE`](./LICENSE) for details.


<!-- Links -->

[Linux]: https://en.wikipedia.org/wiki/Linux
[macOS]: https://en.wikipedia.org/wiki/MacOS
[Windows]: https://en.wikipedia.org/wiki/Microsoft_Windows

[ASDF]: https://asdf.common-lisp.dev/
[Quicklisp]: https://www.quicklisp.org/beta/

[Roswell]: https://roswell.github.io/
[Qlot]: https://github.com/fukamachi/qlot
