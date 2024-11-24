---
title: 🚪 介紹
weight: 0
---

Qob 嘗試在這些事情上做得很好：

- 與 [ASDF][] 及 [Quicklisp][] 搭配使用。
- 像 [Qlot][] 一樣支援專案本機，並支援全局範圍設施。
- 可在所有現代作業系統上運作： [Linux][]、[macOS][]和[Windows][]。

我們的目標是讓 Qob 成為多合一的工具，讓使用者不再需要安裝
[Roswell][] 和 [Qlot][] 之類的工具，這些工具只能達到特定的目標。

## ❓ 為什麼選擇 Qob？

Qob 是一個 Common Lisp 專案管理工具，類似於 Maven 或 Leiningen。
它的目標是控制 Common Lisp 系統的整個生命週期並使其自動化，
包括相依性管理、打包、發行和測試。

- Ruby 專案有 `gemspec` 檔案嗎？
- Node.js 專案有 `package.json` 檔案嗎？
- Clojure 專案有 `project.clj` 檔案嗎？
- Common Lisp 專案有 `Qob` 檔案嗎？

## 📰 消息

請參考[這](https://cl-qob.github.io/Getting-Started/Introduction/#-news).

## 📝 TODO 事項列表

請參考[這](https://cl-qob.github.io/Getting-Started/Introduction/#-todo-list).

## 📂 基礎項目

Qob 的設計深受以下項目的影響：

- [Roswell][] - Common Lisp 環境設定公用程式
- [Qlot][] - Common Lisp 專案本機函式庫安裝程式


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
