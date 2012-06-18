# Opa #

## Overview ##

Opa is an advanced framework for JavaScript, made of two parts:
* A compiler from the Opa language, which features JavaScript-like syntax but introduces many enhancements;
* A JavaScript library, which is used at runtime.

This repository contains all the sources of both the Opa compiler and the Opa library.


## Quick build instructions ##

    $ ./configure
    $ make
    $ sudo make install

should get you there `./configure` should report any missing dependencies and
point you to the packages you may install to fix them (although those may depend
on your distribution). make needs to be GNU make.

The main dependency of Opa is OCaml 3.12 and Node.js 0.6. In case your distribution doesn't
provide a recent enough version, or if you miss some other dependencies, you can
use the `installation_helper.sh` script, which automates the download and
installation of our dependencies. Try

    $ dependencies/installation_helper.sh --help

## Directory layout ##

* \_build: compilation target directory
* build: build scripts and auxiliary Makefiles
* copyright: some copyright information used in our binary package, which
  includes external software (as obtained from the dependency installation
  helper).
* dependencies: contains the dependency installation helper
* dissemination: contains helper programs to run on the cloud (transitional)
* installer: auxiliary files used to build binary packages
* plugins: additional Opa library modules (work in progress)
* stdlib: the Opa standard library
* tools: contains external checking tools; see Credits below
* utils: some auxiliary, independent tools like bash completion, syntax
  highlighting for popular editors, etc.

The other directories contain the source of the Opa libraries, compiler, and
tools.

Files at the root of the repository:

* `*.mllib`: ocamlbuild target declaration for building an ocaml lib
* `*.itarget`: ocamlbuild target declaration for building a set of targets
* `install_release.sh`: builds a full Opa runtime in order to make a binary
  package
* `platform_helper.sh`: some helpers to guarantee script compatibility, mostly
  between Linux and MacOS
* `make_package.sh`: builds various kinds of binary packages from the file
  hierarchy created by `install_release.sh`


## The build system ##

The build system is based on OCamlbuild; the included `build/Makefile.bld`
dynamically builds an ocamlbuild plugin and generates targets based on files
`build_libs`, `build_tools` and `build_rules.ml`.

See `build/Makefile.bld` and `build_rules.ml` for more information. All generated
files are put in `_build`, following the same hierarchy as the source directory.

## License & Credits ##

Opa is open source and Copyright 2007-2012, MLstate.
* The library is released under the MIT license;
* The compiler is released under the GNU Affero General Public License, version 3. 
See file LICENSE for details.

The repository opalang is currently versioning 2 external tools in tools/
used only during the build process of Opa :
* The Closure Compiler (see tools/jschecker/README for copyright information)
* JsDoc Toolkit (see tools/jsdoc-toolkit/README.txt for copyright information)

The Closure Compiler is used as a checker for the Javascript part of the sources
of Opa, and JsDoc Toolkit is used for generating html documentation pages from this
Javascript code.

The use of these tools is transitional, and will soon be made optional.

Part of the jslang library, has been closely inspired by ocamljs/jslib:
* ocamljs/jslib, © 2007-2009 2007-9 Skydeck, Inc, distributed under LGPL v2
In particular files jslang/jsAst.ml and jslang/jsPrint.ml are extended versions of the modules found in ocamljs, specialized for Opa specific needs.
We would like to thank the authors of the ocamljs/jslib.

Opa also includes external libraries :
* Bootstrap, from Twitter - http://twitter.github.com/bootstrap
* Font Awesome - http://fortawesome.github.com/Font-Awesome

Contact
-------

All bug reports, feedback, comments, contributions or remarks are welcome,
either on our website: [http://opalang.org](http://opalang.org).

In particular, users are welcome to:
* subscribe on the mailing list at [https://lists.owasp.org/listinfo/opa](OWASP);
* participate in the forum at [http://forum.opalang.org](opalang).
