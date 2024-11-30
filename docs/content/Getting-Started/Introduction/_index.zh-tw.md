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

對於很多新手來說, 相比其他高階語言像是 Python, Rust, 等等.
Common Lisp 的入門難度是相對算高的. 主要原因並不是因為 Common Lisp
這個語言本身有多難, 困難的地方通常是指它的語言環境,
工具鏈不齊全, 文檔太少或不完善, 網路搜尋時常無果, 等等其他疑難雜症.

Qob 嘗試解決環境不友善的問題. 像是如何建構執行檔, 應該呼叫那些函式
去做那些指令. 不需要進 Lisp 實踐 (ex: SBCL) 也能夠執行一系列的 Lisp 任務.
相比過去 `eval` 需要執行的函式, 使用 Qob 能讓你擁有更接近現代高階程式語言
的使用者體驗.

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
