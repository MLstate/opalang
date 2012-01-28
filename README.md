# OPA #


## Overview ##

Opa is a new generation of cloud development platform. It is a new programming
language, a new web server, a new database and a new distributed execution
engine, all of them tightly integrated to provide a great experience for cloud
developers.

This repository contains all the sources of the OPA compiler and libraries.


## Quick build instructions ##

    $ ./configure
    $ make
    $ sudo make install

should get you there `./configure` should report any missing dependencies and
point you to the packages you may install to fix them (although those may depend
on your distribution). make needs to be GNU make.

The main dependency of OPA is OCaml 3.12. In case your distribution doesn't
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
* plugins: additional OPA library modules (work in progress)
* stdlib: the OPA standard library
* tools: contains external checking tools; see Credits below
* utils: some auxiliary, independent tools like bash completion, syntax
  highlighting for popular editors, etc.

The other directories contain the source of the OPA libraries, compiler, and
tools.

Files at the root of the repository:

* `*.mllib`: ocamlbuild target declaration for building an ocaml lib
* `*.itarget`: ocamlbuild target declaration for building a set of targets
* `install_release.sh`: builds a full OPA runtime in order to make a binary
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

OPA is Copyright 2011, MLstate; it is distributed under the terms of the GNU
Affero General Public License, version 3. See file LICENSE for details.

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

Files jslang/jsAst.ml and jslang/jsPrint.ml are extended versions of the modules
found in ocamljs, specialized for OPA specific needs.
We would like to thank the authors of the ocamljs/jslib.


Contact
-------

All bug reports, feedback, comments, contributions or remarks are welcome,
either on our website: [http://opalang.org](http://opalang.org)

or by email, to:
                              support@opalang.org
