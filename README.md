# Opa #

Opa is an advanced framework for JavaScript, made of two parts:

* A compiler from the Opa language, which features JavaScript-like syntax but introduces many enhancements;
* A JavaScript library, which is used at runtime.

This repository contains all the sources of both the Opa compiler and the Opa library.

## License ##

Opa is open source. The compiler is released under the AGPL license, the library under the MIT license. 

Applications written with Opa only include the library part, so are **free to release applications written with Opa under any proprietary or open source license**.

If you want to create an online "cloud" application platform that includes the Opa compiler, you need to release the sources under the AGPL license as per the AGPL license terms.

Read both [licenses](https://github.com/MLstate/opalang/tree/master/doc). 

## Overview ##

To learn more about Opa, please visit [opalang.org](http://opalang.org) and the [take the tour](https://github.com/MLstate/opalang/wiki/A-tour-of-Opa).

## Getting Started ##

Check out the [dedicated guide](https://github.com/MLstate/opalang/wiki/Getting-started) to install Opa and start to code.

## Quick build instructions ##

    $ ./configure
    $ make
    $ sudo make install

Read more about [building Opa](https://github.com/MLstate/opalang/wiki/Building-Opa).

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

All bug reports, feedback, comments, contributions or remarks are welcome.

In particular, users are welcome to:
* subscribe on the mailing list at [OWASP](https://lists.owasp.org/listinfo/opa);
* participate in the [forum at opalang](http://forum.opalang.org).

For more information, please consult the [opalang portal](http://opalang.org).
