---
title: 🔨 Developing Qob
weight: 20
---

{{< toc >}}

### 🚩 Prerequisites

To make changes to Qob, you should have:

1. [SBCL][] for the lisp implementation.
2. [Make][] for the build system.
3. [Quicklisp][] for CL package manager.

### 📝 Building

To build the development environment, you would have to install Qob using
the [build from source](https://cl-qob.github.io/Getting-Started/Install-Qob/#-build-from-source)
method. Make sure you have set up the environment PATH variable, so you can call
`qob` from the terminal.

After you have stepped through the installation, try:

```sh
$ qob locate
```

It should print out the location of the `qob` executable.
You should be able to identify the Qob executable's location,
even you have multiple Qob versions installed!

### 📈 Testing

Qob does not offer local testing, all our tests are accomplished using GitHub
Actions. Please fork our repository, and push your changes to your fork. GitHub
Actions should pick up the test for you!

Make sure you have GitHub Actions enabled in your repository (forked). Got to
**Settings** -> **Actions** -> **General** -> **Actions Permissions**; make sure
you have checked the correct options.


<!-- Links -->

[SBCL]: https://www.sbcl.org/
[Quicklisp]: https://www.quicklisp.org/beta/

[Make]: https://www.gnu.org/software/make/
