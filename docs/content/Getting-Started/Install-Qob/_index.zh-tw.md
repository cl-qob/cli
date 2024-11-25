---
title: 💾 安裝 Qob
weight: 200
---

本文檔將指導您完成 Qob 的安裝。

安裝 Qob 在 macOS、Linux、Windows、BSD、 等常見的作業系統。

{{< toc >}}

## 💾 預建置檔案

從 [Qob Releases](https://github.com/cl-qob/cli/releases) 下載適合您平台的版本。
下載後，二進製文件可以從任何地方運行。 您無需將其安裝在全球位置。 這適用於您沒有特權帳戶的共享主機和其他系統。

理想情況下，您應該將它安裝在 `PATH` 中的某個位置以便於使用。 `/usr/local/bin` 是最有可能的位置。

## 💾 使用終端

在 macOS 或 Linux:

```sh
$ curl -fsSL https://raw.githubusercontent.com/cl-qob/cli/master/webinstall/install.sh | sh
```

在 Windows:

```sh
$ curl.exe -fsSL https://raw.githubusercontent.com/cl-qob/cli/master/webinstall/install.bat | cmd /Q
```

## 💾 包管理器

### 📦 Nix (macOS 或 Linux)

[Nix][] 是一個適用於 macOS 和 Linux 的免費開源套件管理器。
若要安裝 Qob CLI，請執行下列操作：

```sh
$ nix profile install nixpkgs#qob-cli
```

### 📦 Homebrew (macOS 或 Linux)

[Homebrew][] 是一個適用於 macOS 和 Linux 的免費開源套件管理器。
若要安裝 Qob CLI，請執行下列操作：

```sh
$ brew tap cl-qob/cli https://github.com/cl-qob/packaging
$ brew install qob-cli
```

### 📦 MacPorts (macOS)

[MacPorts][] 是一款適用於 macOS 的免費開源套件管理器。
若要安裝 Qob CLI，請執行下列操作：

```sh
$ sudo port install qob-cli
```

### 📦 Debian (Linux)

Linux [Debian][] 發行版的衍生版本包括 [elementary OS][]、[KDE neon][]、
[Linux Lite][]、[Linux Mint][]、[MX Linux][]、[Pop!_OS][]、[Ubuntu][]、
[Zorin OS][] 等。

```sh
$ sudo curl -SsL -o /etc/apt/trusted.gpg.d/qobsource.gpg https://raw.githubusercontent.com/cl-qob/packaging/master/debian/KEY.gpg
$ sudo curl -SsL -o /etc/apt/sources.list.d/qobsource.list https://raw.githubusercontent.com/cl-qob/packaging/master/debian/qobsource.list
$ sudo apt update --allow-insecure-repositories
$ sudo apt install qob-cli --allow-unauthenticated
```

您也可以直接從 [packaging][packaging/debian] 代碼庫下載 Debian 軟體包。

### 📦 Snap (Linux)

[Snap][] 是一款適用於 Linux 的免費開源套件管理器。
snap 套件適用於大多數發行版，安裝簡單且會自動更新。

```sh
$ sudo snap install qob-cli
```

### 📦 Chocolatey (Windows)

如果您的計算機上安裝了 [Chocolatey][]，則可以使用以下一行代碼安裝 Qob：

```sh
$ choco install qob-cli
```

### 📦 Scoop (Windows)

[Scoop][] 是一個適用於 Windows 的免費開源套件管理器。
若要安裝 Qob CLI，請執行下列操作：

```sh
$ scoop bucket add cl-qob/cli https://github.com/cl-qob/packaging
$ scoop install qob-cli
```

### 📦 Winget (Windows)

[Winget][]是微軟官方的 Windows 免費開源軟體套件管理器。
若要安裝 Qob CLI，請執行下列操作：

```
$ winget install qob.cli
```

## 💾 從原始碼構建

### 🚩 前置工具

- [Git][]
- [SBCL][]
- [Quicklisp][]

或者，您可以直接從這個代碼庫克隆它:

```sh
# 克隆這個代碼庫
$ git clone https://github.com/cl-qob/cli qob-cli

# 將工作目錄更改為 qob-cli
$ cd qob-cli

# 建構執行檔到 `bin` 資料夾
$ make build
```

### 🏡 設定（透過可執行檔）

現在您可以使用可執行檔 `bin/qob` 執行 `qob`；將 `/path/to/qob-cli/bin/` 加入您的環境 `PATH` 以從任何位置執行 qob！

在 Linux/macOS 上、

```sh
export PATH="path/to/qob-cli/bin:$PATH"
```

在 Windows 上、

```batch
set PATH=%PATH%;c:/path/to/qob-cli/bin
```

正確設定後，嘗試 `qob --version` 即可看到目前 `qob` 的版本號碼！🎉 🎊


<!-- Links -->

[packaging/debian]: https://github.com/cl-qob/packaging/tree/master/debian

[Nix]: https://nixos.org/
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
