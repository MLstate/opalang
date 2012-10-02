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

<!--
# Opa Documentation format :

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
 * {R center} [{C
 *   center
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
-->
