---
title: 🔰 Quick Start
weight: 100
---

{{< toc >}}

Using Qob as your Lisp system management tool.

{{< hint info >}}
It is required to have [Git installed](https://git-scm.com/downloads)
to run this tutorial.
{{< /hint >}}

## 🔍 Step 1: Download prebuilt binary

See the [Prebuilt binaries](https://cl-qob.github.io/Getting-Started/Install-Qob/#-prebuilt-binaries) section.

{{< hint ok >}}
💡 If you encounter any issue, try [Build from source](https://cl-qob.github.io/Getting-Started/Install-Qob/#-build-from-source).
{{< /hint >}}

To verify your new installation:

```sh
$ qob --version
```

## 🔍 Step 2: Navigate to an existing project or create a new project

If you already have an existing common lisp project, navigate to the project
root folder.

```sh
$ cd /path/to/project/dir/
```

To create one:

```sh
$ qob create cl-project <your-project>
```

It should create a folder named `<your-project>` in your current working directory.

## 🔍 Step 3: Create `Qob`-file

Then, to create Qob-file in the project:

```sh
$ qob init
```

You should be able to see an `Qob` file in your project folder. 🎉🎊

## 🔍 Step 4: Start the package development

To check your package information, run:

```sh
$ qob info
```

You should be able to see the following information:

```
your-package (1.0.0) | deps: 0
Your project description!
https://example.com/project-url/

Author: Your Name
License: MIT
```

From the start, you would not have any `dependencies` (`0` by default)!

## 🔍 Step 5: Manage system dist

You can manage dist by using the `source` directive in your **Qob**-file.

```cl
(source "quicklisp")  ; default dist
(source "ultralisp")  ; Addition dist
```

{{< hint info >}}
💡 See [DSL/source](https://cl-qob.github.io/DSL/#-source-alias) for more information!
{{< /hint >}}

## 🔍 Step 6: Add some dependencies

You can now add dependencies in your ASD file.

```cl
(defsystem "your-project"
  ...
  :depends-on (flx str clingon)  ; Add dependencies here!
  ...
```

You can add local dependencies by using `depends-on` directive in your **Qob**-file.

```cl
...

(depends-on "fsdb" "https://github.com/billstclair/fsdb" :git)
```

{{< hint danger >}}
💡 Make sure the dependencies you add are available in one of those dist!
{{< /hint >}}

## 🔍 Step 7: Install dependencies

Now we can install the dependencies we have specified in **ASD** and **Qob** files:

```sh
$ qob install-deps
```

You should see Qob executed correctly with the similar output below:

```
Loading ASDF files... done ✓
Installing 1 system...

  - [1/1] Installing fsdb from https://github.com/billstclair/fsdb... done ✓

(Total of 1 system installed; 0 skipped)
Installing 3 systems...

  - [1/3] Installing flx (0)... done ✓
  - [2/3] Installing str (0)... done ✓
  - [3/3] Installing clingon (0)... done ✓

(Total of 3 systems installed; 0 skipped)
```

## 🔗 See Also

- [Commands and options](https://cl-qob.github.io/Getting-Started/Commands-and-options/)
- [Domain Specific Language](https://cl-qob.github.io/DSL/)
- [Basic Usage](https://cl-qob.github.io/Getting-Started/Basic-Usage/)
