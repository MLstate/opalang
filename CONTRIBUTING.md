# Naming conventions:

* the language is called Opa, not OPA or opa;
* respectively, other languages are called JavaScript, OCaml, HTML, PHP;
* the BSL is the Binding System Library;

# Spelling conventions:
* we are writing in American, so use the "z" of your keyboard;
* full words, please, not "we're" but "we are", no "it's" but "it is", etc.

# Chapter conventions:
* tutorials start with "Hello"

# Convention for source code:
* source code must fit in 80 columns;
* source must be fully commented, in Opadoc style;
* keywords are between ++

# Typography:
* don't forget semi-colons;
* in lists, they are compulsory.

# Git commits:
* commit messages must respect the following format:
	* [tag] context: message
* where allowed tags are:
	* cleanup, contrib, debug, doc, enhance, feature, fix, layout, revert.

# ChangeLog conventions:

YEAR.MONTH.DAY, Version SEMVER (stability), GIT_HASH

* change 1
  - description

* change 2

* change 3

# Stdlib

## How to write an Opa module

Read this before writing an Opa module.
If you don't, chances are that the module will not be kept.

The role of these guidelines is to make sure that:

- we can find our way in the code easily
- we produce documentation for end-users
- your changes break neither the compiler nor the user's code.

### I. All code must be documented.

For examples of documented code, see the existing code.
Try to stay as much as possible is the same spirit, and
check that the generated html (opadoc) is well formed.

Documenting takes only a few minutes.
Understanding undocumented code takes hours or days.
If you don't document your code, someone will hate you passionately.
You have been warned.

Important Note: Don't confuse "commenting" and "documenting".
Comments are good but can't replace documentation.
Again, see the examples.

Documentation MUST indicate, among other things
- what the module is for
- who it is for ("@audience PUBLIC"/"@audience PRIVATE"/...)
- possibly, how it is used, by who, in particular for modules of stdlib.core,
because any change in this kind of modules can have an impact on the compiler.

For an example of documentation, see DOCUMENTING, in this directory,
or see all the existing documentation.

### II. How should I call my module? Where should I put it?

We have a notion of packages. The compiler is fully separated with respect to
packages.

The main criterium for the separation of files is the packages dependencies.

Package names can be named with a '.' (dot), but it is a fake hierarchy.  In
fact, a dot can be seen like an underscore, and packages are identifying by
their names, so, if it is not needed, do not go too deep in the hierarchy.

#### II.1) stdlib.core

If your module is used by the compiler, it must go to : stdlib.core
This package is needed for core Opa features, it contains mostly run time
support, like comparison, serialization, servers codes.

This package also defines its own interface for the compiler, i.e. the complete
set of function the compiler can insert calls on. This interface is declared by declaration tagged by @opacapi directives.
Theses declarations should only be toplevel aliases, on functions in stdlib.core,
and the name of each alias must be the full path of the function with dots
replaced by underscores.
E.g. : Value_serialize = Value.serialize

Be extremly aware of the fact that we are trying to reduce as much as possible
the size of the minimal server. This has also an important impact on the debugging.
If your module is not used by the compiler, then, it has really nothing to do in stdlib.core.
If you add a lot of code in stdlib.core, someone will hate you passionately.
You have been warned.

##### II.2) stdlib.*

These packages are additionnaly libs we want to be part of the standard library.
It is still not really clear if a package should be in the stdlib, or not.
Probably most of generic packages developped @mlstate will be in stdlib.*

If your package is too specific, then it is probably better to make a separate package.

some example:
  facebook -> not is stdlib, TODO: specify the package organisation.
  fgraph -> stdlib.fgraph

If you want to add several modules related to the same thing, it would be preferable
to create a new package. (cf e.g. fgraph)
If your module is an extension of an existing package, simply add it in the corresponding package.

### III. Code goes into modules.

Remember, by default, in Opa, everything sits in the current package namespace.
Every typename and every value name you create therefore pollutes the current package namespace,
potentially breaking someone else's code.

Consequently, all your values must be in modules, and/or restricting the interfaces
with the scopes directives :
`@public`
`@package`
`@private`

Similarly, all your types should respect the namespace conventions.
For the moment, we don't have a syntax to put types into record.
Until then, if you wish to add a type "foo" to your module "Foo", call it "Foo.foo".
The name of types should strictly follow the hierarchy of modules.

If you wish to add a function or a type to the global namespace, ask the Opa team first.
Be aware that big changes in the stdlib are dangerous because of the number of potential
users which already use it.
Be aware that if you add a function with a ugly name (or even worth, a function with a
name incoherent with the implementation), someone could use it the next day,
and if you want to change some names, and types after that, it will imply a big refactoring
everywhere your functions are used.

If you add function which does not follow naming conventions, or argument order conventions,
or any other conventions written in this file, someone will hate you passionately.
You have been warned.

Remember that once a Opa distribution will be done, we will no more be able to change any interfaces !
There is no hurry. Take your time. Review your code, ask advice for names of functions to other people,
it is always good to have several point of vues.
See also if a correspond lib does not already exists in an other language.
For exemple, if there is a reference API in an other language, you can keep the same names
(convention used e.g. in fgraph, the API is mostly preserved from OcamlGraph)

Let's say we are playing in a contest of beauty of API.

### IV. Naming conventions.

values_look_like_this
ModuleLookLikeThis
TypesInModule.Look.like_this
fields_look_like_this
files_look_like_this.opa

Accessor functions:

get_foo
is_foo

Conversion functions:

Foo.of_bar
Foo.to_bar

### IV. Types are closed.

When you define a new type or data structure, chances are that users don't need
to know how it's implemented, so you should make it @abstract (if users should
see the type but not its definition) or @private (if the type is purely
internal). This will make error messages simpler for the user and it might make
type-checking a tad faster.

### V. Mutability.

Mutability is bad. You have no idea how bad it is until you've attempted to
execute mutable code in parallel. Consequently, mutable code should be
avoided. Really.

In the current versions of Opa, the unit of mutability is the session. If you
need mutability, use a session. If you need a global state shared by the server,
use [UserContext]. If you need something to be saved for future uses, use the
database. Don't write volatile stuff into the database. No sessions, no session
identifiers, etc.

If you have a use case that fits none of these, and if you are really convinced
that you need a reference, come and see David.  He might scream at you, of
course.

### V. Interacting with the compiler.

If you define a value which needs to be used by the compiler, there are a few
steps you need to take in order to make sure that it won't be removed
prematurely by a compiler optimization and/or by the persons maintaining the
library.

For this purpose, you should

- mark your value as @opacapi
- add it to opacapi/opacapi.ml
- in some cases, add it to the roots
- document how and why it's used in the compiler

### VI. Checking that it works.

Compile with the normal procedure (make), and run tests.

### VII. Bypasses.

Make what you want with bypasses. Do not export private bypasses.

### VIII. Fold, map, etc.

The base loops upon each data structure are

- fold, foldi
- map, mapi
- iter, iteri
- filter, filteri
- reduce, reducei
- filter_map, filter_mapi

Wherever possible, data structures should also support constructors

- unfold, unfoldi
- init
- of_list

And conversion to external data structures

- to_list
- to_map

Unless your data structure doesn't support these constructions, they should all exist, exactly with this name.
If your data structure supports several folds (or several maps, etc), one of them, the default one, must be
called exactly "fold" (respectively "map"). Name the variants with a suffix, which makes the difference explicit
(e.g. "fold" -> "fold_backwards", "iteri" -> "iteri_backwards", etc.)

The order of arguments is the following, keep it consistent for all data structures:

function ( iterator, structure, [more-arguments]) : 'result

- exists:      ( f:('a -> bool),               structure: t('a)               ): bool

- filter:      ( f:'a -> bool,                 structure: t('a)               ): t('a)
- filteri:     ( f:int -> 'a -> bool,          structure: t('a)               ): t('a)
- filter_map:  ( f:'a -> option('b),           structure: t('a)               ): t('b)
- filter_mapi: ( f:int -> 'a -> option('b),    structure: t('a)               ): t('b)

- fold:        ( f:('item, 'acc) -> 'acc,      structure: t('item), init:'acc ): 'acc
- foldi:       ( f:(int, 'item, 'acc) -> 'acc, structure: t('item), init:'acc ): 'acc

- init:        ( f:int -> 'item, size: int):                                     t('item)

- iter:        ( f:('a -> void),               structure: t('a)               ): void
- iteri:       ( f:(int, 'a) -> void,          structure: t('a)               ): void

- map:         ( f:'a -> 'b,                   structure: t('a)               ): t('b)
- mapi:        ( f:(int,'a) -> 'b,             structure: t('a)               ): t('b)

- mem:         ( elt:'a,                       structure: t('a)               ): bool

- reduce:      ( f:'a -> 'a,                   structure: t('a)               ): option('a) //{none} if the structure was empty
- reducei:     ( f:int -> 'a -> 'a,            structure: t('a)               ): option('a) //{none} if the structure was empty

- unfold:      ( f:'counter -> option(('item, 'counter)), init: 'counter      ): t('item)
- unfoldi:     ( f:(int, 'counter)-> option(('item, 'counter)), init: 'counter): t('item)


# Opa Documentation format :

```
/**
 * {1 Tests}
 *
 * [[**] ]
 * [ [ **] ]
 * [ [** ]]
 * [[* *] ]
 * [ [**] ]
 * [ [**] [**] ]
 * [ [ [**] ] ]
 *
 * [[]]
 * [[][]]
 * [[[]]]
 * [{}]
 * [{}{}]
 * [{{}}]
 *
 * [[t[]]]
 *
 * {{}}
 * {{}{}}
 * {{{}}}
 * {[]}
 * {[][]}
 * {[[]]}
 *
 * {[{t{}}}
 * {[{% \emph{Hello \latex}}}
 *
 * {1 Syntax of documentation comments}
 *
 * Comments in an Opa file are parsed by opadoc to produce associated
 * documentation. A special syntax can be used within these comments in order
 * to organize the produced documentation and make it look better.
 *
 * You can either write independent documentation comments or associate them to
 * code elements (functions, modules...) by placing them just before the
 * element they describe.
 *
 * {2 Structure}
 *
 * You can use the following tags to organize the documentation into different
 * parts.
 *
 * Square and curly brackets can be used inside formatting brackets without
 * being escaped as long as they go in pair (opening and closing ones). If
 * you'd like to use a single opening or closing bracket, you should escape it
 * with a backslash.
 *
 * {3 Headings}
 *
 * {[
 *   {1 Heading1}
 *   {2 Heading2}
 *   {3 Heading3}
 *   {4 Heading4}
 *   {5 Heading5}
 *   {6 Heading6}
 * }
 *
 * {3 Text formats}
 *
 * {4 Basic styles}
 *
 * {b bold}: [{b bold}]
 *
 * {it italic}: [{it italic}]
 *
 * {emp emphasize}: [{emp emphasize}]
 *
 * {^ superscript}: [{^ superscript}]
 *
 * {_ subscript}: [{_ subscript}]
 *
 * {4 Alignments }
 *
 * {C center} [{C
 *   center
 * }]
 * {L left} [{L
 *   left
 * }]
 * {R right} [{R
 *   right
 * }]
 *
 * {4 Code}
 *
 * {[
 *   {[
 *     ...
 *     your code here
 *     ...
 *   }
 * }
 *
 * {4 Verbatim}
 *
 * {[
 *   {v
 *     ...
 *     verbatim text here
 *     ...
 *   }
 * }
 *
 * {4 Lists}
 *
 * {ul ul}
 *
 * {enum enum}
 *
 * {4 Raw LaTeX}
 *
 * {[
 *   {% \emph{Hello \latex}}
 * }
 *
 * {4 Custom}
 *
 * {[
 *   {custom custom tag}
 * }
 */
```
