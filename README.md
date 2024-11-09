<a href="#"><img src="./docs/static/logo.png" width="20%"></a>
> CLI for building, running, testing, and managing your Common Lisp dependencies

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![SBCL Version](https://img.shields.io/badge/SBCL-2.2.9+-white.svg?logo=awslambda&logoColor=white)](https://www.sbcl.org/)
[![Release](https://img.shields.io/github/tag/cl-qob/cli.svg?label=release&logo=github)](https://github.com/cl-qob/cli/releases/latest)

Qob attempts to be good at these things:

- Play nicely with [ASDF][] and [Quicklisp][].
- Support project local like [Qlot][] and support global scope facilities.
- Works on all modern operating systems: [Linux][], [macOS][], and [Windows][].
- No extra file required, qlfile, `.ros` files, etc.

We aim to make Qob an all-in-one tool so users no longer need to install tools like [Roswell] and [Qlot][], which only serve specific goals.

## 🚧 State of the project

We are currently in a *early-development* phase.  API, and commands are not stable.

Things might still break at any point.

## 🔨 Development

You need the following softwares:

- [SBCL][]
- [Make][]

then,

```sh
$ make build
```

## 🔗 Links

- [Documentation](https://cl-qob.github.io/)
- [Installation](https://cl-qob.github.io/Getting-Started/Install-Eask/)
- [Command-line interface](https://cl-qob.github.io/Getting-Started/Commands-and-options/)
- [Examples](https://cl-qob.github.io/Examples/Real-project-examples/)
- [FAQ](https://cl-qob.github.io/FAQ/)

## 🧪 Testing

We have incorporated a range of tests to ensure Qob remains stable throughout its release cycle.

###### Documentation

| Description                            | Done | Status                                                                                                                                   |
|----------------------------------------|------|------------------------------------------------------------------------------------------------------------------------------------------|
| Keep the documentation page up to date | ✔    | [![Docs](https://github.com/cl-qob/cli/actions/workflows/docs.yml/badge.svg)](https://github.com/cl-qob/cli/actions/workflows/docs.yml) |

###### Development

| Description       | Done | Status                                                                                                                                      |
|-------------------|------|---------------------------------------------------------------------------------------------------------------------------------------------|
| Build executables | ✔    | [![Build](https://github.com/cl-qob/cli/actions/workflows/build.yml/badge.svg)](https://github.com/cl-qob/cli/actions/workflows/build.yml) |

###### Others

| Description | Done | Status                                                                                                                                                     |
|-------------|------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Webinstall  | ✔    | [![Webinstall](https://github.com/cl-qob/cli/actions/workflows/webinstall.yml/badge.svg)](https://github.com/cl-qob/cli/actions/workflows/webinstall.yml) |

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

[SBCL]: https://www.sbcl.org/

[ASDF]: https://asdf.common-lisp.dev/
[Quicklisp]: https://www.quicklisp.org/beta/

[Roswell]: https://roswell.github.io/
[Qlot]: https://github.com/fukamachi/qlot

[Make]: https://www.gnu.org/software/make/
