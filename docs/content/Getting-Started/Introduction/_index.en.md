---
title: 🚪 Introduction
weight: 0
---

Qob attempts to be good at these things:

- Play nicely with [ASDF][] and [Quicklisp][].
- Support project local like [Qlot][] and support global scope facilities.
- Works on all modern operating systems: [Linux][], [macOS][], and [Windows][].

We aim to make Qob an all-in-one tool so users no longer need to install
tools like [Roswell][] and [Qlot][], which only serve specific goals.

## ❓ Why Qob?

For many newcomers, getting started with Common Lisp can feel relatively
easy compared to other advanced languages like Python or Rust.
However, it is also perceived as a challenging language to begin with.
This difficulty doesn't stem from the language itself but rather from the
ecosystem: the environment, toolchain, sparse documentation, limited search
results, and other complexities can create obstacles.

Qob aims to address these challenges by making the environment more accessible.
It simplifies tasks such as building executables and determining which functions
to call for specific commands. Qob allows users to perform various Lisp
operations without directly interacting with a Lisp implementation (e.g., SBCL).
By using Qob, you can enjoy a user experience closer to that of modern
high-level programming languages, avoiding the reliance on the traditional
`eval`-based workflows.

## 📰 News

- `0.1.x` - Project bare-bones are pretty much complete!

## 📝 Todo list

### 🔍 Core commands

- [ ] [FEAT] Add `publish` command; to publish the system to the Quciklisp dist?

### 🔍 Qob-file commands

- N/A

## 📂 Underlying Projects

The design of Qob was greatly influenced by the following projects:

- [Roswell][] - Common Lisp environment setup Utility
- [Qlot][] - A project-local library installer for Common Lisp


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
