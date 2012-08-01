(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   Definition of register directives for building external primitives libraries.

   @author Mathieu Barbin
*)

(**
   This module defines the type of the preprocess directives you can find in
   the different files given to {b bslregister}, for example :
   {[
   ##register myfun : int, string -> string
   ]}

   As a complement, see also the module [BslTags.t] which manage only the optionnal
   parameters for directives :
   {[
   ##register [tag:attribute] ....
   like in :
   ##register [noprojection] ....
   ]}

   For extanding tags, it is really easy, you just have to modify 1 file : BslTags.

   For extanding directives, it is a little bit more work, you have to modify this
   file, the parser of course, and all implementation of Generation for any language.
   - BslOcaml
   - BslJs
*)

(** {6 Alias, just for lisibility} *)

type pos              = FilePos.pos
type source_code      = string
type filename         = string
type regexp           = string
type injected         = bool
type bslkey           = string
type path             = string
type skey             = string



(** {6 Generic Types} *)
(**
   The generic type of a directive.

   Tags are some extra options you can give to the directive,
   as in
   {[
   ##mydirective [mytag1, mytag:with_1_argument] directive_contents....
   ]}
*)

type ('tags, 'directive) decorated_source_elt =
  | Directive    of pos * 'tags * 'directive
  | Source       of pos * source_code


type ('tags, 'directive) decorated_file = {
  filename              : filename ;
  decorated_source      : ('tags, 'directive) decorated_source_elt list
}

(** {6 Positions} *)

(**
   This ast follows the guidelines about positions,
   for error reporting.
*)

(** *)
let pos = LangAst.pos
let reset_pos = LangAst.reset_pos
let merge_pos = LangAst.merge_pos


(** {6 GUIDELINES for matching a [decorated_source_elt]} *)
(**

   Verbose:
   {[
   | Directive (pos, tags, directive)
   | Source (pos, source)
   ]}

   Shorter:
   {[
   | Directive (p, t, d)
   | Source (p, s)
   ]}
*)


(** {6 Opa files directives} *)
(**
   Note:
   There are no position in this AST, because such a directive
   is always wrapped into a [decorated_source_elt] which already
   provides the position.
*)
(** *)
type opalang_directive =
  | IncludeType          of regexp
  | Include              of BslIncludeFormats.fmt * path
  | FormatDefinition     of string

(**
   Specialization of the generic [decorated_file] for opa files.
*)
type opalang_decorated_file =
    (BslTags.t, opalang_directive) decorated_file

(**
   {9 IncludeType}

   A directive for producing all types definitions matching a regexp.
   A typical example, is to include all type from a given path

   {[
   ##extern-type mymodule.*
   ]}
   is represented as :
   {[
   IncludeType "mymodule.*"
   ]}

   {9 Include}

   Inclusion preprocess is a mechanism for producting directly code from
   bypass definitions, without rewritting the code with the type manually.

   TODO: document format, cf BslIncludeFormat.
   {[
   ##include <format> path
   ]}

   {9 FormatDefinition}
   TODO: documentation
*)

(** {6 GUIDELINES for matching a [opalang_directive]} *)
(**

   Verbose:
   {[
   | IncludeType regexp
   | Include (iformat, path)
   | FormatDefinition name
   ]}

   Shorter:
   {[
   | IncludeType rgx
   | Include (ifmt, p)
   | FormatDefinition n
   ]}
*)


(** {6 Bypass files directives} *)
(**
   Note:
   There are no position in this AST, because such a directive
   is always wrapped into a [decorated_source_elt] which already
   provides the position.
*)
(** *)
type bypasslang_directive =
  | ExternalTypeDef      of skey * BslTypes.typevar list * source_code option
  | OpaTypeDef           of skey * BslTypes.typevar list
  | Module               of skey * source_code option
  | EndModule
  | Register             of skey * source_code option * injected * BslTypes.t
  | Args                 of skey * (string * BslTypes.t) list * BslTypes.t
  | Property             of BslTags.parsed_t

(**
   Specialization of the generic [decorated_file] for Ocaml and Javascript files.
*)
type bypasslang_decorated_file =
    (BslTags.t, bypasslang_directive) decorated_file

(**
   {9 ExternTypeDef}

   A extern type definition.

   This makes actually sence only in Ocaml files.
   It introduces a new ocaml type.

   {[
   ##extern-type ('a, 'b, 'c) toto = Ka of 'a | Kb of 'b | Kc of 'c
   ]}
   is represented as :
   {[
   ExternTypeDef ("toto", ['a ; 'b ; 'c], "Ka of 'a | Kb of 'b | Kc of 'c")
   ]}

   @see "BslTypesGeneration.Ocaml" to see how this type is defined in opa, and how it is printed
   in the [ml] and [mli] of the generated [MLRuntime]

   {9 Module}

   {[
   ##module skey \ impl_name
   ]}
   is represented as :
   {[
   Module ("skey", "impl_name")
   ]}

   {9 EndModule}

   For closing a module previously open.

   Syntax:
   {[
   ##endmodule
   ]}

   {9 Register}

   For defining a new primitive.
   + The name of the key can be different than the name of the primitive in the
   target language.
   + The primitive can be injected from an existing implementation. In this case,
   the implementation will be directly injected in the generated code. (cf Syntax 3)

   Syntax 1:
   {[
   ##register skey : int, int -> int
   ]}
   is represented as :
   {[
   Register ("skey", "skey", false, BslTypes.(int, int -> int))
   ]}

   Syntax 2:
   {[
   ##register skey \ different_name : int, int -> int
   ]}
   is represented as :
   {[
   Register ("skey", "different_name", false, BslTypes.(int, int -> int))
   ]}

   Syntax 3:
   {[
   ##register skey \ `Pervasives.(+)` : int, int -> int
   ]}
   is represented as :
   {[
   Register ("skey", "Pervasives.(+)", true, BslTypes.(int, int -> int))
   ]}

   This is the only case where the injected bool is set to [true]

   {9 Args}

   TODO: documentation for args.
   [impl], [arg_name, arg_type], return_type

   {9 Property}

   TODO: documentation for property
*)


(** {6 GUIDELINES for matching a [bypasslang_directive]} *)
(**

   Verbose:
   {[
   | ExternalTypeDef (skey, params, implementation)
   | Module (skey, implementation)
   | EndModule
   | Register (skey, implementation, injected, bslty)
   | Args (name, args, bslty)
   | Property (props)
   ]}

   Shorter:
   {[
   | ExternalTypeDef (n, p, imp)
   | Module (n, imp)
   | EndModule
   | Register (n, imp, inj, ty)
   | Args (n, xs, ty)
   | Property p
   ]}
*)
