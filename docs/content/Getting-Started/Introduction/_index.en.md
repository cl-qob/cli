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

Qob is an Common Lisp project management tool, similar to Maven or Leiningen.
It aims to control and automate the entire life cycle of an Common Lisp system,
including dependency management, packaging, distribution and testing.

- Ruby projects have a `gemspec` file?
- Node.js projects have a `package.json` file?
- Clojure projects have a `project.clj` file?
- Common Lisp projects have a `Qob` file?

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
