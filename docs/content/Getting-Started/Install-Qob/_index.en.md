---
title: 💾 Install Qob
weight: 200
---

This document guides you through the installation of Qob.

Install Qob on macOS, Linux, Windows, BSD, and on any machine that can run the [Node.js][].

{{< toc >}}

## 💾 Prebuilt binaries

Download the appropriate version for your platform from [Qob Releases](https://github.com/cl-qob/cli/releases).
Once downloaded, the binary can be run from anywhere. You don’t need to install
it in a global location. This works well for shared hosts and other systems
where you don’t have a privileged account.

Ideally, you should install it somewhere in your `PATH` for easy use. `/usr/local/bin`
is the most probable location.

## 💾 Using Shell

On macOS or Linux:

```sh
$ curl -fsSL https://raw.githubusercontent.com/cl-qob/cli/master/webinstall/install.sh | sh
```

On Windows:

```sh
$ curl.exe -fsSL https://raw.githubusercontent.com/cl-qob/cli/master/webinstall/install.bat | cmd /Q
```

## 💾 Package managers

### 📦 Homebrew (macOS or Linux)

[Homebrew][] is a free and open-source package manager for macOS and Linux.
To install the Qob CLI:

```sh
$ brew tap cl-qob/cli https://github.com/cl-qob/packaging
$ brew install qob-cli
```

### 📦 MacPorts (macOS)

[MacPorts][] is a free and open-source package manager for macOS.
To install the Qob CLI:

```sh
$ sudo port install qob-cli
```

### 📦 Debian (Linux)

Derivatives of the [Debian][] distribution of Linux include [elementary OS][],
[KDE neon][], [Linux Lite][], [Linux Mint][], [MX Linux][], [Pop!_OS][],
[Ubuntu][], [Zorin OS][], and others.

```sh
$ sudo curl -SsL -o /etc/apt/trusted.gpg.d/qobsource.gpg https://raw.githubusercontent.com/cl-qob/packaging/master/debian/KEY.gpg
$ sudo curl -SsL -o /etc/apt/sources.list.d/qobsource.list https://raw.githubusercontent.com/cl-qob/packaging/master/debian/qobsource.list
$ sudo apt update --allow-insecure-repositories
$ sudo apt install qob-cli --allow-unauthenticated
```

You can also download Debian packages from the [packaging][packaging/debian] repo.

### 📦 Snap (Linux)

[Snap][] is a free and open-source package manager for Linux.
Available for most distributions, snap packages are simple to install and are
automatically updated.

```sh
$ sudo snap install qob-cli
```

### 📦 Arch (Linux)

There's a `PKGBUILD` that builds `qob` from sources and creates a package, so
inside the top directory of the repository you can simply run:

```sh
$ makepkg -i
```

### 📦 Chocolatey (Windows)

If you have [Chocolatey][] installed on your machine, you can
install Qob with the following one-liner:

```sh
$ choco install qob-cli
```

### 📦 Scoop (Windows)

[Scoop][] is a free and open-source package manager for Windows.
To install the Qob CLI:

```sh
$ scoop bucket add cl-qob/cli https://github.com/cl-qob/packaging
$ scoop install qob-cli
```

### 📦 Winget (Windows)

[Winget][] is Microsoft’s official free and open-source package manager for Windows.
To install the Qob CLI:

```
$ winget install qob.cli
```

## 💾 Build from source

### 🚩 Prerequisite Tools

- [Git][]
- [SBCL][]
- [Quicklisp][]

Alternatively, you can clone it directly from this repo

```sh
# clone the repo
$ git clone https://github.com/cl-qob/cli qob-cli

# change the working directory to qob-cli
$ cd qob-cli

# build executable to `bin` folder
$ make build
```

### 🏡 Setup (through executable)

You can now run `qob` using the executable `bin/qob`; add `/path/to/qob-cli/bin/`
to your environment `PATH` to execute qob from any location!

On Linux/macOS,

```sh
export PATH="path/to/qob-cli/bin:$PATH"
```

On Windows,

```batch
set PATH=%PATH%;c:/path/to/qob-cli/bin
```

Once you have set it up correctly, try `qob --version` then you should see
the current `qob`'s version number! 🎉 🎊


<!-- Links -->

[packaging/debian]: https://github.com/cl-qob/packaging/tree/master/debian

[Homebrew]: https://brew.sh/
[MacPorts]: https://www.macports.org/
[Snap]: https://snapcraft.io/
[Chocolatey]: https://chocolatey.org/
[Scoop]: https://scoop.sh/
[Winget]: https://learn.microsoft.com/en-us/windows/package-manager/

[Git]: https://git-scm.com/
[SBCL]: https://www.sbcl.org/
[Quicklisp]: https://www.quicklisp.org/beta/

[Debian]: https://www.debian.org/
[elementary OS]: https://elementary.io/
[KDE neon]: https://neon.kde.org/
[Linux Lite]: https://www.linuxliteos.com/
[Linux Mint]: https://linuxmint.com/
[MX Linux]: https://mxlinux.org/
[Pop!_OS]: https://pop.system76.com/
[Ubuntu]: https://ubuntu.com/
[Zorin OS]: https://zorin.com/os/
