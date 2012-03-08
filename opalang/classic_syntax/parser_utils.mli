(*
    Copyright © 2011, 2012 MLstate

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
   Functions used by the parser to generate code

   Index:
   - General functions
   - Error functions
   - Hints utils
   - Generating fresh stuff
   - Dealing with labels
   - Tuples and records
   - Identifier
   - Constants
   - Lambda, application
   - Re tuple and records, for generating code while duplicating labels
   - Generating datastructures
   - Generating other AST nodes
   - Call to OPA functions, that are supposed to be defined in the stdlib
   - Utilities on patterns
   - Utils on type constants
   - Utils on type vars
   - General buiders for types
   - Various typenames
   - Type utils for directives
   - Utils for building directives
   - Utils for parser_xml
   - Utils for CSS
   - Utils for inlined CSS
   - Utils for trx parser
   - Utils to duplicate a tree while regenerating all the annotations

*)

open QmlLoc
open SurfaceAst

(* mli and variants are a nightmare *)
type ('a,'b) coerced_expr = ('a, [> `coerce ] as 'b) expr
type ('a,'b) coerced_expr_node = ('a, [> `coerce ] as 'b) expr_node

(* Variant types are a nightmare without smart alias ;) *)

(** An expression resulting of parsing *)
type parsing_expr = (nonuid, parsing_directive) expr

(** An node resulting of parsing *)
type parsing_node = (nonuid, parsing_directive) expr_node

(** Activate sugar mode to preserve syntactic sugar inside a parsing directive *)
val set_sugar_mode : unit -> unit

(** General functions *)
val cur2 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val unc2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c


(** Error functions *)
exception Specific_parse_error of (FilePos.pos * string)

val const_ty_to_string : string ty_node -> string
val isolate : string -> string
val warning1 : string -> annot -> unit
val error1 : string -> annot -> 'a
val error_comment : annot -> 'a
val error_string : annot -> 'a
val error_char_escape : char -> annot -> 'a
val error_fun_space : annot -> 'a
val error_directive_not_good_arguments : string * annot -> 'a
val error_static_record : string * annot -> 'a
val error_static_string : string * annot -> 'a
val error_not_a_directive : string * annot -> 'a
val error_directive_number_argument : int -> ?max:int -> int -> string * annot -> 'a
val error_directive_wrong_arguments_type : string * annot -> 'a
val error_char_overflow : annot -> 'a
val error_int_overflow : annot -> 'a
val error_neither_ident_nor_call : annot -> 'a
val error_redefinition_basic_type : string ty -> 'a
val error_bad_compare : annot -> 'a
val error_static_xml : annot -> 'a
val error_consecutive_arrow : annot -> 'a
val error_sliced_expr : annot -> 'a
val error_db_file_without_slash : string * annot -> 'a
val error_conflicting_type_def_visibility :
  (SurfaceAst.type_def_visibility * SurfaceAst.type_def_visibility * annot) ->
  'a

(** Hints utils *)
(* used to warn of possible mistakes *)
type hint = [ `function_call of annot | `declaration of annot | `same_indent of (annot * annot) | `same_indents of annot list ]
val push_hint : hint -> unit
val clear_hints : unit -> unit
val print_hints : unit -> unit

 (* a reference to the name of the file currently being parsed (needed as id for FilePos) *)
val filename : string ref

(** Generating fresh stuff *)
val fresh_id : unit -> int
val fresh_variable : unit -> string
val fresh_name : ?name:string -> unit -> string


(** Dealing with labels *)
val annot : FilePos.pos -> annot
val builtin: unit -> annot
(* [pos_only filename offset_start offset_stop] *)
val pos_only : string -> int -> int -> annot
val decorate : string -> 'a -> int -> int -> 'a label
val undecorate : 'a * annot -> 'a
val label : 'a * annot -> annot
val copy_label : annot -> annot
(* unlike [label], the label here is copied *)
val nlabel : 'a * annot -> annot
val union_annot : annot -> annot -> annot
val union_annot2 : 'a * annot -> 'b * annot -> annot
val union_annot_list : ('a * annot) list -> annot
val union_annot_list_snd : ('a * ('b * annot)) list -> annot
val wrap : (annot -> 'a) -> annot -> 'a * annot
val wrap_e : ('a * annot -> 'b) -> 'a * annot -> 'b * annot
val map_annot : ('a -> 'b) -> 'a * annot -> 'b * annot


(** Tuples and records *)
(* return a record/pat record/type record*)
val encode_tuple_as_record : (string option * 'a) list -> (string * 'a) list
val encode_tuple : 'a list -> (string * 'a) list
val encode_record : (string * 'a) list -> (string * 'a) list
val encode_tuple_pos : ('a * annot) list -> (string * ('a * annot)) list * annot
val encode_record_pos : (string * ('a * annot)) list -> (string * ('a * annot)) list label
val encode_args_as_record : 'a list -> (string * 'a) list
val encode_args_as_record_pos : ('a * annot) list -> (string * ('a * annot)) list * annot


(** Identifier *)
val var_to_patvar : 'a * annot -> 'a pat
val var_to_exprvar : 'a * annot -> ('a, 'c) expr
val patident : 'a -> annot -> 'a pat
val patvar : 'a -> annot -> 'a pat
val ident : 'a -> annot -> ('a, 'c) expr
val fresh_ident_pat : annot -> (string, 'a) expr * string pat


(** Constants *)
val void_pat : annot -> 'a pat
val void : annot -> ('a,'b) coerced_expr
val true_pat : annot -> 'a pat
val true_ : annot -> ('a,'b) coerced_expr
val false_pat : annot -> 'a pat
val false_ : annot -> ('a,'b) coerced_expr
val string : string -> annot -> ('a, 'b) expr
val string2 : string * annot -> ('a, 'b) expr
val float2 : float * annot -> ('a, 'b) expr
val float : float -> annot -> ('a, 'b) expr
val int2 : int * annot -> ('a, 'b) expr
val int : int -> annot -> ('a, 'b) expr
val floatint2 : int * annot -> ('a, 'b) expr
val intfloat2 : float * annot -> ('a, 'b) expr
val bool : bool -> annot -> ('a, 'b) coerced_expr


(** Lambda, application *)
val get_meaningful_called_ident : (string, 'b) expr -> string
val args_expr_to_lambda : ?zero_ary:annot -> 'a pat list -> ('a, 'b) expr -> ('a, 'b) expr
val ( & ) : ('a, 'b) expr -> ('a, 'b) expr list -> ('a, 'b) expr
val ( &. ) : ('a, 'b) expr -> ('a, 'b) expr list -> ('a, 'b) expr_node
val apply_f_with_holes : (string, 'a) expr -> [< `expr of (string, 'a) expr | `hole of annot ] list -> (string, 'a) expr
val function_ : (string pat * (string, 'a) expr) list -> annot -> (string, 'a) expr_node
val lambda_to_lambda : ((string, 'a) expr -> (string, 'b) expr) -> annot -> (string, 'b) expr
val lambda : string -> (string, 'a) expr -> (string, 'a) expr_node
val make_function2 :
  ((string * (string, parsing_directive) expr) list ->
  (string, parsing_directive) expr -> (string, parsing_directive) expr) ->
  ((string, parsing_directive) expr ->
   string -> (string, parsing_directive) expr) ->
  [< `expr of (string, parsing_directive) expr | `hole of annot ] ->
  ([< `dot of string
    | `double_dot of string
    | `function_call of
        [< `expr of (string, parsing_directive) expr | `hole of annot ]
        list &
        [< `expr of (string, parsing_directive) expr | `hole of annot ] list ] *
   annot)
  list ->  (string, parsing_directive) expr_node

val apply_operators :
  [< `left | `nonassoc | `right ] ->
  [ `expr of (string, 'a) expr | `hole of annot ] *
  ((string, 'a) expr *
   [ `expr of (string, 'a) expr | `hole of annot ])
  list -> (string, 'a) expr


(** Re tuple and records, for generating code while duplicating labels *)
val tuple_nocons : ('a,'b) expr list -> ('a,'b) record
val tuple : ('a, 'b) expr list -> ('a, 'b) expr
val record_nocons : (string * ('a,'b) expr) list -> ('a,'b) record
val record : (string * ('a,'b) expr) list -> ('a,'b) expr
val tuple_pat_nocons : 'a pat list -> 'a pat_record_node * annot
val tuple_pat : 'a pat list -> 'a pat
val record_pat_nocons : (string * 'a pat) list -> 'a pat_record_node * annot
val record_pat :'a pat_record_node -> 'a pat
val simple_record : string -> annot -> ('a, 'b) coerced_expr_node
val record1 : string -> ('a, 'b) expr -> ('a, 'b) expr
val simple_record_expr : string -> annot -> ('a, 'b) coerced_expr
val simple_record_expr2 : string * annot -> ('a, 'b) coerced_expr
val default_value_in_expr_record : bool ->
  [< `binding of 'a * (string, [> `coerce ] as 'c) SurfaceAst.expr
   | `noassign of 'a *
                  [< `novalue of string * QmlLoc.annot
                  | `value of (string, 'c) SurfaceAst.expr ] *
                  string SurfaceAst.ty option ] list ->
      ('a * (string, 'c) SurfaceAst.expr) list
val default_value_in_pat_record : bool ->
 (string * [< `novalue of string * QmlLoc.annot
           | `value of string SurfaceAst.pat ] * string SurfaceAst.ty option) list ->
     (string * string SurfaceAst.pat) list
val default_value_in_type_record : bool ->
 (string * [< `novalue of string * QmlLoc.annot
           | `value of string SurfaceAst.ty ]) list ->
     (string * string SurfaceAst.ty) list


(** Generating datastructures *)
val list_nil : annot -> (string, 'b) coerced_expr
val list_nil_pat : annot -> string pat
val list_cons : (string, [> `coerce ] as 'b) expr -> (string, 'b) expr -> (string, 'b) expr
val list_cons_pat : string pat -> string pat -> string pat
val some : (string, 'b) expr -> (string, 'b) coerced_expr
val none : annot -> (string, 'b) coerced_expr
val option_expr_of_expr_option : (string, 'b) expr option -> annot -> (string, 'b) coerced_expr
val list_pat_of_pat_list :
  ?tl:(string pat) -> string pat list -> annot -> string pat
val list_expr_of_expr_list :
  ?tl:(string,'a) expr -> (string, [> `coerce ] as 'a) expr list -> annot -> (string, 'a) expr
val list_expr_of_expr_list_no_coerce :
  ?tl:(string,'a) expr -> (string, [> `coerce ] as 'a) expr list -> annot -> (string, 'a) expr
val list_expr_of_expr_list_tail_coerce :
  ?tl:(string,'a) expr -> (string, [> `coerce ] as 'a) expr list -> annot -> (string, 'a) expr
val list_expr_of_expr_openlist :
  ?tl:(string,'a) expr -> (string, [> `coerce | `opensums] as 'a) expr list -> annot -> (string, 'a) expr
val list_expr_of_expr_list_unsafe : (string, [> `coerce ] as 'a) expr list -> (string, 'a) expr(**Fails if the list is empty*)
val list_constructors_of_string_list: (string * annot) list -> annot -> (string, [> `coerce ]) expr

(** Generating other AST nodes *)
val letins : ('a * ('a, 'b) expr) list -> ('a, 'b) expr -> ('a, 'b) expr
val letin : 'a -> ('a, 'b) expr -> ('a, 'b) expr -> ('a, 'b) expr
val dot : ('a, 'b) expr -> string -> ('a, 'b) expr
val dot_path : ('a,'b) expr -> string list -> ('a,'b) expr
val dots : string list -> annot -> (string, 'a) expr
val dots2 : (string * annot) list -> (string, 'a) expr
val applys : ('a, 'b) expr -> ('a, 'b) expr list -> ('a, 'b) expr
val apply : ('a, 'b) expr -> ('a, 'b) expr -> ('a, 'b) expr

(** Call to OPA functions, that are supposed to be defined in the stdlib *)
val append : (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr
val stringmap_empty : annot -> (string, 'a) expr
val cssentrymap_empty : annot -> (string, 'a) expr
val stringmap_add : annot -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr
val cssentrymap_add : annot -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr
val map_add_merge : annot -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr
val db_write : (string, 'a) expr -> (string, 'a) expr -> (string, 'a) expr_node
val dom_transform : (string, [> `coerce ] as 'a) expr -> (string, 'a) expr_node

(** Utilities on patterns *)
val if_then_else : ('a, 'b) coerced_expr -> ('a, 'b) expr -> ('a, 'b) expr option -> ('a, 'b) expr_node
(* transform [let pat = expr in expr] into [match expr with pat -> expr] *)
val bind_in_to_expr_in :
  (parsing_directive * (string, parsing_directive) expr list * string ty list) label list ->
  string pat * (string, parsing_directive) expr ->
  (string, parsing_directive) expr -> (string, parsing_directive) expr_node
val pat_in_to_simple_bindings : (string pat * ((string, [< all_directives > `coerce `recval ]) expr as 'expr)) -> (string * 'expr) list

val binding_to_pattern_binding : ('a * 'b) * 'c -> ('a SurfaceAst.pat_node * 'b) * 'c

(** Utils on type definitions *)
val merge_type_def_visibility:
  SurfaceAst.type_def_visibility list -> QmlLoc.annot ->
    SurfaceAst.type_def_visibility

(**
   Merge options (directives) of typedefs.
   The global is the options read before the 'type' keyword,
   which is global to all type defs, in case of a type ... and
   construction.
   The local is the options just given to 1 type defs.
   {[
   @global type toto = ...
   and @local tata = ...
   ]}
   This function merge the options, or raise an error in case
   of incompatible options.
*)
val merge_type_def_options :
  global:QmlAst.ty_def_options ->
  local:QmlAst.ty_def_options ->
  QmlAst.ty_def_options

(** Utils on type constants *)
val type_const : const_ty_node -> annot -> 'a ty
val tyint : annot -> 'a ty
val tyfloat : annot -> 'a ty
val tystring : annot -> 'a ty
val tyvoid_ : 'a ty_node
val tyvoid : annot -> 'a ty

(** Utils on type vars *)
val tyvar : 'a -> annot -> 'a ty
val fresh_tyvar : annot -> string ty
val colvar : 'a -> annot -> 'a sum_t
val alpha : annot -> string ty
val beta : annot -> string ty

(** General buiders for types *)
val row_t_tuple : ?rowvar:'a rowvar -> 'a ty list -> 'a row_t
val row_t_record : ?rowvar:'a rowvar -> 'a fields_t_node -> 'a row_t
val row_t : 'a ty list -> annot -> 'a row_t
val arrow_t_node : 'a ty list -> 'a ty -> annot -> 'a arrow_t_node
val arrow_t : 'a ty list -> 'a ty -> annot -> 'a arrow_t
val arrow : 'a ty list -> 'a ty -> annot -> 'a ty_node
val arrow2 : 'a ty -> 'a ty -> annot -> 'a ty_node
val typenamed : 'a -> 'a ty list -> annot -> 'a ty
val row_label : string ty -> annot -> string row_t
val row : string ty -> _ * annot -> string row_t
val tuple_type : ?rowvar:'a rowvar -> 'a ty list -> 'a ty
val record_type : ?rowvar:'a rowvar -> 'a fields_t_node -> 'a ty
val tuple_string : 'a list -> string
val tuple_name : 'a list -> annot -> string ty

(** Various typenames *)
val tyxhtml : annot -> string ty
val tyxml : annot -> string ty
val tytext : annot -> string ty
val tybool : annot -> string ty
val tylist : string ty -> annot -> string ty
val tyaction : annot -> string ty
val tyopaty: annot -> string ty


(** Utils for building directives *)
val coerce : ('b, [> `coerce ] as 'a) expr -> 'b ty -> ('b, 'a) expr_node
val coerce_expr : ('b, [> `coerce ] as 'a) expr -> 'b ty -> ('b, 'a) expr
val may_coerce_expr : (string, [> `coerce ] as 'a) expr -> string ty option -> (string, 'a) expr
val may_coerce_pat : string pat -> string ty option -> string pat
val coerce_name : (string, [> `coerce ] as 'a) expr -> string -> (string, 'a) expr_node
val coerce_name_expr : (string, [> `coerce ] as 'a) expr -> string -> (string, 'a) expr
val coerce_name_pat : string pat -> string -> string pat
val directive0 : 'a -> unit -> ('b, 'a) expr_node
val directive1 : 'a -> ('b, 'a) expr -> ('b, 'a) expr_node
val directive1' : (('c,'b) expr -> 'b) -> ('c, 'b) expr -> ('c, 'b) expr_node
val directive2 : 'a -> ('b, 'a) expr * ('b, 'a) expr -> ('b, 'a) expr_node
val directive2' : ('a -> 'b) -> 'a * ('c, 'b) expr -> ('c, 'b) expr_node
val parser_ : 'b -> ('a,[> `parser_ of 'b]) expr_node
val module_ : ('a,[> `module_] as 'b) expr -> ('a,'b) expr_node
val open_ : ('a, [> `open_ ] as 'b) expr list -> ('a, 'b) expr -> ('a, 'b) expr
val xml_parser : 'a -> ('b, [> `xml_parser of 'a ]) SurfaceAst.expr_node
val magic_to_string : (string, [> `magic_to_string ] as 'a) expr -> (string, 'a) expr_node
val magic_to_xml : (string, [> `magic_to_xml ] as 'a) expr -> (string, 'a) expr_node
val magic_to_text : (string, [> `magic_to_text ] as 'a) expr -> (string, 'a) expr_node
val magic_do : (string, [> `magic_do] as 'a) expr -> (string, 'a) expr
val computed_string : (string, [> `string] as 'a) expr list -> (string, 'a) expr_node

val toplevel_opens : ('a, [> `toplevel_open ] as 'b) expr list -> ('a, 'b) code_elt_node list
val bypass : string -> ('a, 'b) expr_node
val double_dot : (string, 'a) expr -> string -> (string, 'a) expr
val declaration_directive :
  (parsing_directive * (string, parsing_directive) expr list * string ty list) label list ->
  ((_ * (string, parsing_directive) expr) list as 'a) -> 'a
val declaration_directive_1 :
  (parsing_directive * (string, parsing_directive) expr list * string ty list) label list ->
  ((_ * (string, parsing_directive) expr) as 'a) -> 'a


(** Utils for parser_xml *)
type 'b dom_tag_args = {
  args : (string * string * (string, 'b) expr) list;
  class_ : (string, 'b) expr option;
  style : (string, 'b) expr option;
  xmlns_declaration: (string * (string, 'b) expr) list;
  events : ((string, 'b) expr) list;
  events_expr : (string, 'b) expr option;
  events_options: ((string, 'b) expr) list;
  href:    (string, 'b) expr option
} constraint 'b = [> `coerce ]
val action :
  string ->
  (string, [> `coerce | `magic_to_string | `magic_to_xml ] as 'a)
  expr ->
  (string * [< `identity | `magicToString | `magicToXml ] * (string * (string, 'a) expr) list option) * annot ->
  string * annot ->
  (string, 'a) expr -> (string, 'a) expr_node
val action_content :
  parsing_expr -> [< `append | `prepend | `set ] * QmlLoc.annot -> parsing_expr ->
  parsing_expr
val action_css :
  parsing_expr -> [<`set] * QmlLoc.annot -> parsing_expr ->
  parsing_expr
val appendlo : (string, 'a) expr -> (string, 'a) expr option -> (string, 'a) expr
val css_build : string * annot -> (string, 'a) expr list -> (string, 'a) expr
val css_build_unsafe : string -> (string, 'a) expr list -> (string, 'a) expr
val css_build1 : string -> annot -> (string, 'a) expr
val css_build1' : string -> 'a * annot -> (string, 'b) expr
val map_tuple4 : ('a -> 'b) -> 'a * 'a * 'a * 'a -> 'b * 'b * 'b * 'b
val empty_args : annot -> _ dom_tag_args
val arg_default : 'a dom_tag_args option * annot -> 'a dom_tag_args
val create_fragment : (string, [> `coerce ] as 'a) expr list -> annot -> (string, 'a) expr
val create_textnode : string * annot -> (string, [> `coerce ]) expr
val create_fragment_with_spaces : ((string * annot) option * (string, [> `coerce ] as 'a) expr) list * annot -> (string, 'a) expr
val create_empty_fragment : annot -> (string, [> `coerce ]) expr
val create_element :
  (string * annot) * (string * annot) ->
  'a dom_tag_args ->
  (string, 'a) expr list ->
  (string, 'a) expr
val tag_mismatch : (string * annot) * (string * annot) -> (string * annot) * (string * annot) -> unit
val add_arg :
  'a dom_tag_args option * annot ->
  (string * annot) * (string * annot) ->
  (string, 'a) expr option -> 'a dom_tag_args
val to_handle : string * annot -> (string, [> `coerce ]) expr
val add_event :
  'a dom_tag_args option * annot ->
  string * annot ->
  (string, 'a) expr -> 'a dom_tag_args

(**Set the options for an event (e.g. [stop_propagation], etc.)*)
val add_event_option:
  'a dom_tag_args option * annot ->
  string * annot ->
  (string, 'a) expr -> 'a dom_tag_args

val add_events : 'b dom_tag_args option * annot -> (string, 'b) expr -> 'b dom_tag_args
val action_xml : (string, [> `coerce | `fun_action ] as 'a) expr -> (string, 'a) expr


(**
   Concatenate a sequence of xml characters (represented as strings of length 1),
   removing any meaningless whitespace
*)
val concat_xml_text: string list -> string

(**
   Define the variables which may be necessary for xhtml.
*)
val around_xhtml: (string, 'a) expr -> (string, 'a) expr
val around_xmlns: (string, 'a) expr -> (string, 'a) expr

type xml_or_xhtml = Xml | Xhtml
val push_xml : xml_or_xhtml -> unit
val pop_xml : unit -> unit
val xhtml_mode : unit -> bool

val push_tag: string -> unit
val pop_tag:  unit   -> unit
val get_tag:  unit   -> string

(** Utils for CSS *)
val hexa2 : char * annot -> char * 'a -> ('b, 'c) expr
val rgba : ('a, 'b) expr -> ('a, 'b) expr -> ('a, 'b) expr -> ('a, 'b) expr -> ('a, 'b) expr
val color_hexa :
  char * annot ->
  char * annot ->
  char * annot ->
  char * annot ->
  char * annot ->
  char * annot ->
  char * annot ->
  char * annot -> ('e, 'f) expr
val rgb :
  ('a, 'b) expr ->
  ('a, 'b) expr ->
  ('a, 'b) expr -> ('a, 'b) expr


(** Utils for inlined CSS *)
val conv :
  [< `bottom
   | `center
   | `left
   | `right
   | `size of ('a, 'b) coerced_expr
   | `top ] *
  annot -> ('a, 'b) expr
val background_position :
  [< `bottom
   | `center
   | `left
   | `right
   | `size of ('a, 'b) coerced_expr & 'c
   | `top
   > `left `right ] *
  annot ->
  ([< `bottom
    | `center
    | `left
    | `right
    | `size of ('a, 'b) coerced_expr
    | `top
    > `bottom `size `top ] *
   annot) option -> ('a, 'b) expr * ('a, 'b) expr
val hyphen_to_underscore : string -> string
val css_build_with_prefix : string -> string * annot -> (string, 'a) expr
val simple_record_hyphen2 : string * annot -> ('a, 'b) coerced_expr
val css_simple_record_hyphen2 : string * annot -> ('a, 'b) coerced_expr
val list_style_def : string * annot -> ('a, 'b) coerced_expr


(** utils for rewriting deep record syntactic sugar *)
val rewrite_long_extend_record :
  ((string * annot) list * (string, SurfaceAst.parsing_directive) SurfaceAst.expr) list ->
  (string, SurfaceAst.parsing_directive) SurfaceAst.expr ->
  (string, SurfaceAst.parsing_directive) SurfaceAst.expr

val rewrite_letin: bool -> (nonuid * parsing_expr) list -> parsing_expr -> parsing_node
val rewrite_add_recval : parsing_expr -> parsing_expr
