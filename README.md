# Opa #

## Overview ##

Opa is an advanced framework for JavaScript, made of two parts:

* A compiler from the Opa language, which features JavaScript-like syntax but introduces many enhancements;
* A JavaScript library, which is used at runtime.

This repository contains all the sources of both the Opa compiler and the Opa library.

## Getting Started ##

Check out the [dedicated guide](https://github.com/MLstate/opalang/wiki/Getting-Started).

## Quick build instructions ##

    $ ./configure
    $ make
    $ sudo make install

should get you there. `./configure` should report any missing dependencies and
point you to the packages you may install to fix them (although those may depend
on your distribution). `make` needs to be GNU make.

The main dependency of Opa is OCaml 3.12 and Node.js >= 0.6. In case your distribution doesn't provide a recent enough version, or if you miss some other dependencies, you can use the `installation_helper.sh` script, which automates the download and
installation of our dependencies. Try

    $ tools/dependencies/installation_helper.sh --help

#### Amazon Image

We provide a ready to use [Amazon Image for Opa](https://github.com/MLstate/opalang/wiki/Amazon-Image-for-Opa).

## Directory layout ##

* \_build: compilation target directory
* compiler: compiler source files
* doc: documentation source files, guidelines, and miscellaneous copyright information
* lib: Opa standard library, plugins and binding system library source files
* ocamllib: OCaml libraries and runtime source files
* tools: various tools

Details of the lib directory:

* stdlib: the Opa standard library
* plugins: additional Opa library modules
* opabsl: the Opa binding system library, for binding various backends (currently node and OCaml)
* experimental: some experimental Opa libraries

Details of the doc directory:

* copyright: some copyright information used in our binary package, which
  includes external software (as obtained from the dependency installation
  helper)
* manual.omd: source of the Opa manual available in various languages (currently English and Chinese) viewable at [doc.opalang.org](http://doc.opalang.org)
* manual.src: source code of the examples of the manual
* `*.omd`: custom markdown file format (aka. Opa markdown)
* `*-LICENSE`: licenses used by the Opa project

Details of the tools directory:

* build: build scripts and auxiliary Makefiles
* dependencies: contains the dependency installation helper and various patches
* dissemination: contains helper programs to run on the cloud (transitional)
* installer: auxiliary files used to build binary packages
* tools: contains external checking tools
* bash: bash completion scripts
* editors: various helper tools for popular editors
* utils: packaging scripts, and various auxiliary tools

Other files usage:

* `*.mllib`: ocamlbuild target declaration for building an ocaml lib
* `*.itarget`: ocamlbuild target declaration for building a set of targets
* `tools/platform_helper.sh`: some helpers to guarantee script compatibility, mostly
  between Linux and MacOS
* `tools/utils/install_release.sh`: builds a full Opa runtime in order to make a binary package
* `tools/utils/make_package.sh`: builds various kinds of binary packages from the file
  hierarchy created by `tools/utils/install_release.sh`

## The build system ##

The build system is based on OCamlbuild; the included `tools/build/Makefile.bld`
dynamically builds an ocamlbuild plugin and generates targets based on files
`tools/build/build_libs`, `tools/build/build_tools` and `tools/build/build_rules.ml`.

See `tools/build/Makefile.bld` and `tools/build/build_rules.ml` for more information. All generated files are put in `_build`, following the same hierarchy as the source directory.

You can find more information about the build system in tools/build/README.

Contact
-------

All bug reports, feedback, comments, contributions or remarks are welcome: [http://opalang.org](http://opalang.org).

In particular, users are welcome to:
* subscribe on the mailing list at [https://lists.owasp.org/listinfo/opa](OWASP);
* participate in the forum at [http://forum.opalang.org](opalang).
