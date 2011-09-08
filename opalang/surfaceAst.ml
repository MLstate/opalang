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
   The OPA Surface AST.

   @author David Rajchenbach-Teller
   @author Rudy Sicard
*)

(**
   {5 Definitions}
*)
type 'a label = 'a QmlLoc.label

(**
   {6 Identifiers}

   Not-quite-phantom types used to guarantee that we never confuse
   unique identifiers and not-made-unique-yet identifiers.
*)


type uids = Ident.t
(**
   A key to determine the (non)-equality of identifiers.

   This is useful as the same source code can often contain two
   identical identifiers (say [x]) with different definitions (say
   [let x = 1 in let x = 2 in e]). While here both identifiers will
   be called [x], they will have distinct hashes.
*)

type nonuid = string
    (**A name as parsed in the source*)

let hash : string -> string -> uids =
  fun name description -> Ident.next ~descr:description name
  (** Generate a unique hash for a name.

      Usage: [hash name description]. *)

let string_of_hash : uids -> string = Ident.stident

(**
   {6 Expressions}
*)

(**
   {7 Other expressions}
*)
type bind_directives = QmlAst.userland_visibility_directive (* TODO Move expand here *)

(**
   Internal data structures.

   Not all of these data structures are visible in the concrete syntax.
   For instance, the concrete syntax offers [text] (Unicode ropes)
   instead of [string] (character-neutral array-style strings).
*)
type const_expr_node =
  | CInt    of Big_int.big_int
  | CFloat  of float
(*  | CFixed  of fixed*)(**This is actually a user-level data structure.*)
  | CString of string   (**A UTF-8 encoded string.*)

and const_expr = const_expr_node label (* cf libqmlcompil/qmlLoc.ml *)

type ('ident, 'dir) record      = ('ident, 'dir) record_node label
and  ('ident, 'dir) record_node = (string * ('ident, 'dir) expr) list

(**
   Type of an expression

   @param 'ident The type of identifiers: either [nonuid] until identifiers have been made unique or [uids] once they have
   @param 'dir The type of directives, i.e. stuff left by the compiler for a further phase to rewrite.
*)
and ('ident, 'dir) expr = ('ident, 'dir) expr_node label
and 'ident bind_ident = {ident :'ident ; directives : bind_directives list}
and ('ident, 'dir) expr_node =
  | Apply        of ('ident, 'dir) expr * ('ident, 'dir) record
      (**
         We may add a support for labeled arguement.
         For now, those records are tuples ("f1", "f2", etc.)
      *)

  | Lambda       of 'ident pat_record_node * ('ident, 'dir) expr
      (**
         cf remark for Apply node, about labeled argument.
      *)

  | Const        of const_expr_node
  | Ident        of 'ident
  | LetIn        of bool (* rec *) * ('ident * ('ident, 'dir) expr) list * ('ident, 'dir) expr
  | Match        of ('ident, 'dir) expr * ('ident pat * ('ident, 'dir) expr) list
  | Record       of ('ident, 'dir) record_node
  | ExtendRecord of ('ident, 'dir) record_node * ('ident, 'dir) expr (**[ExtendRecord r e] extends
                                                               the result of [e] with the
                                                               fields defined in [r].*)
  | Dot          of ('ident, 'dir) expr * string
  | Bypass       of BslKey.t (**A primitive, handled through the Bypass Standard Library*)

  | DBPath       of ('ident, 'dir) dbelt * QmlAst.Db.kind
  | Directive    of ('ident, 'dir) directive

      (**
         Instruction for a later compilation phase.

         Usage: [Directive (directive, human_readable_name, lazy expr, how_to_type)].

         [human_readable_name] is used only for pretty-printing

         To obtain the type of the expression, apply [how_to_type] to the
         type of [lazy expr] (only if lazy_expr is a one element list).
         Expressions which should not be typeable (typically because they
         are expected to be rewritten away before reaching the typer) should
         have type None.
*)
and ('ident, 'dir) directive =  'dir * ('ident, 'dir) expr list * 'ident ty list

(**
   {6 Database}
*)

and ('ident, 'dir) dbelt      = ('ident, 'dir) dbelt_node label
and ('ident, 'dir) dbelt_node = ('ident, 'dir) preprocessed_db_element list

and ('ident, 'dir) preprocessed_db_element = ('ident, 'dir) preprocessed_db_element_node label
and ('ident, 'dir) preprocessed_db_element_node =
  | FldKey  of string
  | ExprKey of ('ident, 'dir) expr (* not [expr_node], because consecutive labels, but unequal positions, because of brackets *)
  | NewKey

(**
   {6 Pattern}
*)

and 'ident pat = 'ident pat_node label

and 'ident pat_node =
  | PatRecord       of 'ident pat_record_node * QmlAst.pat_rowvar
  | PatAny
  | PatConst        of const_expr_node
  | PatVar          of 'ident bind_ident
  | PatCoerce       of 'ident pat * 'ident ty
  | PatAs           of 'ident pat * 'ident bind_ident

and 'ident pat_record_node = (string * 'ident pat) list

(**
   {6 Types}
*)

and 'ident ty = 'ident ty_node label
and 'ident ty_node =
    | TypeConst    of const_ty_node
    | TypeVar      of 'ident typevar
    | TypeArrow    of 'ident arrow_t_node
    | TypeRecord   of 'ident row_t_node
    | TypeSumSugar of 'ident sum_t list
    | TypeNamed    of 'ident typeinstance_t_node
    | TypeExternal
    | TypeForall   of 'ident typeforall
    | TypeModule   of 'ident fields_t_node

and 'ident typeforall = 'ident typevar list * 'ident ty

and 'ident typeinstance_t= 'ident typeinstance_t_node label
and 'ident typeinstance_t_node = 'ident typeident * 'ident ty list

and 'ident arrow_t       = 'ident arrow_t_node label
and 'ident arrow_t_node  = 'ident row_t * 'ident ty (**The type of a function.*)

and 'ident sum_t         = 'ident sum_t_node label
and 'ident sum_t_node    =
  | SumName   of 'ident typeinstance_t_node
  | SumRecord of 'ident row_t_node (* warning: the typer won't be able to deal with row variables in columns *)
  | SumVar    of 'ident colvar

and 'ident fields_t_node = (string * 'ident ty) list

and 'ident row_t         = 'ident row_t_node label
and 'ident row_t_node    = TyRow of 'ident fields_t_node * 'ident rowvar option

and 'ident typevar       = Flatvar   of 'ident(**Type variables, e.g. ['a], ['b], etc.*)
and 'ident typeident     = Typeident of 'ident(**Type identifiers, e.g. [list], [int]*)

and const_ty_node =
  | TyInt
  | TyFloat
  | TyString

and 'ident rowvar        = Rowvar of 'ident
and 'ident colvar        = Colvar of 'ident

(**
   {6 Declarations}
*)

and ('ident, 'dir) code_elt = ('ident, 'dir) code_elt_node label
and ('ident, 'dir) code_elt_node =
  | Database   of 'ident * string list * QmlAst.Db.options list
  | NewDbDef   of (('ident, 'dir) expr, 'ident ty) QmlAst.Db.db_def
  | NewType    of 'ident typedef list
  | NewVal     of ('ident pat * ('ident, 'dir) expr) list * bool (* rec *)
      (* after dependency analysis, toplevel mutually recursive functions
       * are regrouped in a NewVal *)
  | Package of [`declaration | `import | `import_plugin] * string

and type_def_visibility =
  | TDV_public   (** Type definition is public, visible from anywhere. *)
  | TDV_abstract   (** Type definition is visible from anywhere but internal
           representation is only visible inside the hosting package. Since
           at parsing stage we don't know yet the currently compiled package,
           the name of the package is not set and its determination is delayed
           upon we create a QML visibility information. *)
  | TDV_private   (** Type definition is not exported outside the hosting
           package, i.e. doesn't appear in the package's interface. Same remark
           than above about the package name. *)

and 'ident typedef = 'ident typedef_node label
and 'ident typedef_node = {
  ty_def_options : QmlAst.ty_def_options ;
  ty_def_visibility : type_def_visibility ;
  ty_def_name : 'ident typeident ;
  ty_def_params : 'ident typevar list ;
  ty_def_body :'ident ty
}

type ('ident, 'dir) code = ('ident, 'dir) code_elt list (**One (or more) complete source file(s)*)


(**
   {5 The ast for pattern matching on xml }
 *)
type 'expr namespace = {namespace : 'expr ; name : string label}
type 'expr xml_suffix =
  | Xml_star
  | Xml_plus
  | Xml_question
  | Xml_number of 'expr
  | Xml_range of 'expr * 'expr
type 'expr xml_pattern_attribute_value =
  | XmlExists
  | XmlName
  | XmlAttrStringParser of 'expr
  | XmlAttrParser of 'expr
type 'expr xml_pattern_attribute =
  (* string is a unique name used by the parser generator *)
  'expr namespace * string option * 'expr xml_pattern_attribute_value
type 'expr xml_pattern =
  | XmlLetIn of (string * 'expr) list * 'expr xml_pattern (* this node allows to bind namespaces *)
  | XmlExpr of 'expr
  | XmlNode of 'expr namespace *
            'expr xml_pattern_attribute list *
            'expr xml_named_pattern list
  | XmlAny
  | XmlParser of 'expr Trx_ast.item list (* no disjunction allowed to avoid parsing ambiguities *)
    (* should we bring a node XmlSuffix -> we can write this in the syntax
       already anyway
       but then what about <toto a={e}>*
    *)
and 'expr xml_named_pattern = string option * 'expr xml_pattern * 'expr xml_suffix label option
and 'expr xml_rule = 'expr xml_named_pattern list * 'expr (* one line of parser *)
type 'expr xml_parser =
  'expr xml_rule list (* the alternatives *)


(**
   {5 Various shorthands for directives}
*)
type magic_directive =
    [ `magic_to_string
    | `magic_to_xml

    | `magic_do
        (**
           this directive is no longer used for executing a list of funaction,
           this is just there for keeping the 'do' syntax when we reprint a parsed
           opa code.
           this directive is removed during the transformation into QmlAst.
        *)

    | `typeof
    | `specialize of [ `strict | `polymorphic ]
    ]

type string_directive = [ `string ]

type internationalization_directive = [
    | `i18n (* indicate a point of translation *)
    | `i18n_lang (* return the current context lang, add a directive to later prune js code pattern matching at running time *)
]

type error_directive =
    [ `assert_
    ]
type coding_directive = [
  | `deprecated
  | `todo
]
type insert_server_directive =
    [ `server_entry_point
    ]
type concurrency_directive =
    [ `spawn
    | `wait
    | `callcc
    | `atomic
    | `thread_context
    | `with_thread_context
    | `no_client_calls
    | `throw
    | `catch
    | `may_cps
    | `async
    ]
type distribution_directive = QmlAst.slicer_directive
type file_inclusion_directive =
    [ `static_content  of string (*Relative file name*)* bool (*[true] if a [string] is expected, [false] if a [-> string] is expected*)
    | `static_resource of string (*Relative file name*)
    | `static_content_directory  of string (*Relative file name*)* bool (*[true] if a [string] is expected, [false] if a [-> string] is expected*)
    | `static_resource_directory of string (*Relative file name*)
    ]
type access_directive =
    [ `private_ (* visible only in the current module *)
    | `public (* visible to everyone *)
    | `package (* visible only in the current package *)
    ]
type hack_directive =
    [ `unsafe_cast
    | `fail
    | `tracker of PassTracker.t
    | `expand of Big_int.big_int option
    | `compiletime of string (* see pass_CompileTimeDirective *)
    | `opacapi (* see Opacapi, and checkopacapi *)
    ]
type type_directive =
    [ `coerce
    | `unsafe_cast
    | `nonexpansive
    | `opensums
    | `openrecord
    | `module_
    | `module_field_lifting
    | `warncoerce
    ]
type other_directive =
    [ `fun_action
    | `js_ident
    | `sliced_expr (** the expressions is a two elements containing first the client expression and then the server expression  *)
    | `llarray
        (** cf doc in QmlAst *)
    | `recval (** see QmlAst *)
    ]
type alpha_renaming_directive =
    [ `open_ (** not used anymore *)
    | `toplevel_open
    | `module_
    | `toplevel
    ]

(**
   path * access
*)
type documentation_directive =
    [ `doctype of string list * QmlAst.doctype_access_directive ]

type opavalue_directive = [
| `stringifier
| `comparator
| `serializer
| `xmlizer
]

type basic_directive =
    [ magic_directive
    | string_directive
    | internationalization_directive
    | coding_directive
    | error_directive
    | concurrency_directive
    | file_inclusion_directive
    | hack_directive
    | type_directive
    | other_directive
    | documentation_directive
    | insert_server_directive
    | opavalue_directive
    | `create_lazy_record
    | distribution_directive
    ]
(** these directives are the ones that are not taken care of in the surfaceAst
    they go straight to qml (or fail at the conversion when not implemented)
    If you are adding a directive that needs to go though opa to be taken care of
    in qml, it must end up in this type
*)



type dependency_directive =
    [ basic_directive
    | access_directive
    | `local of uids ]

type renaming_directive =
    [ access_directive
    | basic_directive
    | alpha_renaming_directive ]
type parsing_directive =
    [ `xml_parser of (string, parsing_directive) expr xml_parser
    | `parser_ of (string, parsing_directive) expr Trx_ast.expr
    | renaming_directive ]

type all_directives =
    [ parsing_directive
    | dependency_directive
    | renaming_directive ]
