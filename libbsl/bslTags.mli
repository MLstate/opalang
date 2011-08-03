(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(**
   Managment of tags for a bypass in a specific implementation.

   @author Mathieu Barbin
*)

(**
   This module contains a type [BslTags.t] to represent bypass tags,
   with a documentation of what are tags for, and a easy way to extend them.
*)

(** {6 Tags : Attributes for bypases} *)

(** The type, as it is parsed by bslregister *)
type parsed_t = ( string * string option ) list

(**
   The type for bsltags.

   + [backend:restriction] : UNDOCUMENTED -- default is [None]
   + [internal_first] : HACK will be removed. internal tag of BslRegisterLib.
   is true if the function is the first of its module-record (js) -- default is [false]
   + [js_module_representation] : the way module are produced in js.
   new_object seems to be deprecated -- default is [None]
   + [no_projection] : [None]: this bypass should never be projected, [Some stringset]:
   this bypass should be projected for everybody but inhabitants of the set
   <!> use at your own risk ! -- default is [false]
   + [opaname] : tag for type definition, meant not to apply the standardization
   on the name of the type.
   + [restricted] : the bypass is not meant to be used by a user, but only generated in a compiler pass.
   The optional string list bring some more restriction.
   If the bypass has been taged with [restricted:toto],
   only a pass insering a [Directive (`restricted_bypass "toto", [Bypass _], _)] is authorized to use it.
   Beware, [Some (Some []) <> Some None]. -- default is [None]
   + [second_order] is a tag for bypass which contains an arrow type in one of their argument, or the returned type.
   + [cps_bypass] is a tag for bypass that take explicitly the cps continuation.
   + [opacapi] says that this bypass is part of the compiler interface (inserted by some pass)

   This tag is added by the bslregister process, so that the type should not be inspected each time we may need to
   get this information.
*)
type t =
    {
      backend_restriction : StringSet.t option ;
      no_projection : StringSet.t option ;
      opaname : bool ;
      raise_ : bool ;
      restricted : string list option option ;
      second_order : bool ;
      cps_bypass : bool ;
      opacapi : bool ;
    }

(** The default tags. See default values in the definition of the type [t] *)
val default : t

(** Test if a value of type [t] is physically equal to the [default] value
    ( computed with [t == default] )
    this is used to optimize the generated code of loaders *)
val is_default : t -> bool

(** {6 Printing} *)

type 'a pprinter = 'a LangPrint.pprinter
val pp : t pprinter
val pp_tag : (string * string option) pprinter

(**
   Meta printing, for code generation.
   Print as Ocaml concrete syntax
*)
val pp_meta_tag : (string * string option) pprinter
val pp_meta : parsed_t pprinter

(** {6 Error reporting } *)

(** Error during parsing of tags *)
type error
exception Exception of error
val pp_error : error pprinter

(** {6 Parsing} *)

(**
   The tags are found with the implementation, in the file processed by {b bslregister}.
   The syntax to insert tags is the following :
   [##register [mytag, myothertag, mytagwith:an_argument] etc...]

   In {b bslregister}, the parser does not try to parse explicitly the tags. It builds just
   a row type from all tags read in the files : [ (string * string option) list ] : tag * optional argument.

   Then, the [parse] function of this module is called to build a element of type [BslTags.t].

   That means that you can easily extend the type [BslTags.t] by modifying only this module.
*)

(**
   Parse the given list of tags.

   If a pos is provided, do not raise the error, but directly fails with OManager.
   if not,
   @raise Error with an appropriate error message, in case of unbound tags, conflict, etc.. *)
val parse : ?pos:FilePos.pos -> parsed_t -> t

(**
   Reversing the parsing, returns back the parsed_t
*)
val parsed_t : t -> parsed_t

(** {6 Helpers for projection semantic} *)

type passname = string

(** Given the name of a pass, and a [BslTags.t], says if the projection should be done *)
val do_projection : t -> passname -> bool

(** Given  a [BslTags.t], says if the projection cannot be projected
    (ie declared as [no_projection]) *)
val never_projected : t -> bool

(** A pretty message to get in mind the complex semantic of the field [no_projection] of a [BslTags.t].
    @param no_projection The field [no_projection] as it is in the tags [BslTags.t] corresponding to this bypass
*)
val string_of_no_projection : StringSet.t option -> string

(** {6 Helpers for checking bypass restriction} *)

(**
   The bypass restriction is used to check whenever a bypass is used outside of
   the context it is meant to be :
   + if a user try to use a bypass reserved for an internal use only (inserted by the compiler)
   + if a pass try to insert a bypass restricted for an other pass.
*)

(** A pretty message to get in mind the complex semantic of the field [restricted] of a [BslTags.t].
    @param restricted The field [restricted] as it is in the tags [BslTags.t] corresponding to this bypass
*)
val string_of_restricted : string list option option -> string

(** This function implements the semantic of bypass restriction.

    @param restriction The restriction of the bypass. This is [None] if the bypass is represented
    with [Bypass] in the ast, and [Some pass] if it is [Directive (`restricted_bypass pass, _, _)]
    @param tags The tags [BslTags.t] corresponding to this bypass
    @return [true] if and only if the bypass is authorized
 *)
val authorized_bypass : restriction:string option -> t -> bool

(** Same than [authorized_bypass] but with an other interface for commodity :
    [authorized_bypass_as_expr restricted bypass]
    @param tags The tags [BslTags.t] corresponding to this bypass
    @param bypass A [QmlAst.expr] which must be either [Directive (`restricted_bypass _, [Bypass _], _)]
    or [Bypass _]
    @return [true] if and only if the bypass is authorized
 *)
val authorized_bypass_as_expr : t -> QmlAst.expr -> bool
