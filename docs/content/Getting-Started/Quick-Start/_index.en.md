---
title: 🔰 Quick Start
weight: 100
---

{{< toc >}}

Using Qob as your Lisp system management tool.

{{< hint info >}}
The installation are not cross-platform, using [npm](https://www.npmjs.com/).
For instructions about how to install Qob with other methods, see
[install](https://cl-qob.github.io/Getting-Started/Install-Qob/).

It is required to have [Git installed](https://git-scm.com/downloads)
to run this tutorial.
{{< /hint >}}

## Step 1: Download prebuilt binary

See the [Prebuilt binaries](https://cl-qob.github.io/Getting-Started/Install-Qob/#-prebuilt-binaries) section.

{{< hint ok >}}
💡 If you encounter any issue, try [Build from source](https://cl-qob.github.io/Getting-Started/Install-Qob/#-build-from-source).
{{< /hint >}}

To verify your new installation:

```sh
$ qob --version
```

## Step 2: Navigate to an existing project or create a new project

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

## Step 4: Create `Qob`-file

Then, to create Qob-file in the project:

```sh
$ qob init
```

You should be able to see an `Qob` file in your project folder. 🎉🎊

## Step 5: Start the package development

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

From the start, you would not have any `dependencies` and `devDependencies` (`0` by default)!

## Step 6: Manage package archives

You can manage package archives by using the `source` directive in your **Eask**-file.

```elisp
(source "gnu")    ; default
(source "melpa")  ; Add package archives
```

{{< hint info >}}
💡 See [DSL/source](https://emacs-eask.github.io/DSL/#-source-alias) for more information!
{{< /hint >}}

## Step 7: Add some dependencies

You can add dependencies by using `depends-on` directive in your **Eask**-file.

```elisp
...

(depends-on "f")
(depends-on "ht")
```

{{< hint danger >}}
💡 Make sure the dependencies you add are available in the package archives!

Or else you would get an error **`package-name-' is unavailable**!
{{< /hint >}}

## Step 8: Install dependencies

Now we can install the dependencies we have specified in the **Eask**-file:

```elisp
$ eask install-deps
```

You should see Eask executed correctly with the similar output below:

```
Loading package information... done
Installing 2 package dependencies...
  - Installing f (20220405.1534)... done
  - Installing ht (20210119.741)... done

(Total of 2 dependencies installed, 0 skipped)
```

## See Also

- [Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
- [Domain Specific Language](https://emacs-eask.github.io/DSL/)
- [Basic Usage](https://emacs-eask.github.io/Getting-Started/Basic-Usage/)
