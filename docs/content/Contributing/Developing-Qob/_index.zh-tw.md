---
title: 🔨 開發 Qob
weight: 20
---

{{< toc >}}

### 🚩 必備條件

要更改 Qob，您應該：

1. [SBCL][] 為 lisp 實作。
2. [Make][] 用於編譯系統。
3. [Quicklisp][] 為 CL 套件管理員。

### 📝 建構

若要建立開發環境，您必須使用 [build from source](https://cl-qob.github.io/Getting-Started/Install-Qob/#-build-from-source)
方法安裝 Qob。請確定您已設定環境 PATH 變數，因此您可以從終端機呼叫 `qob`。

完成安裝後，嘗試：

```sh
$ qob locate
```

它應該會列印出 `qob` 可執行檔的位置。即使您已安裝多個 Qob 版本，也應該可以辨識 Qob 可執行檔的位置！

### 📈 測試

Qob 不提供本地測試，我們所有的測試都是使用 GitHub 完成的動作。 請 fork 我們的存儲庫
並將您的更改推送到您的 fork。 GitHub 行動應該為你拿起測試！

確保您的存儲庫（分叉）中啟用了 GitHub Actions。 必須 **設置** -> **操作** -> **常規**
-> **操作權限**； 確保您已經檢查了正確的選項。


<!-- Links -->

[SBCL]: https://www.sbcl.org/
[Quicklisp]: https://www.quicklisp.org/beta/

[Make]: https://www.gnu.org/software/make/
