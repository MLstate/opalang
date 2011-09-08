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
open SurfaceAst
open QmlLoc (* brings annot into scope *)

module type IDENT =
  sig
    type ident
    val equal : ident -> ident -> bool
    val val_ : string -> ident
    val typ : string -> ident
    val fresh : unit -> ident (* typevar only *)
    val ns_fresh : label:QmlLoc.annot -> string -> ident (* for any (expression) identifiers *)
  end

module type CONS =
sig
  type ident
  type ident' = ident

  module I : IDENT with type ident = ident'

  (** @inline doc *)
  (* Types *)
  module T :
  sig
    val name :        ?label:annot -> ?tyl:ident ty list -> string -> ident ty
    val row_t :       ?label:annot -> ?row:'a rowvar -> 'a fields_t_node -> 'a row_t
    val args :        ?label:annot -> ident ty list -> ident row_t
    val arrow :       ?label:annot -> ident ty list -> ident ty -> ident arrow_t
    val arrow_1 :     ?label:annot -> ident ty -> ident ty -> ident arrow_t
    val arrow_2 :     ?label:annot -> ident ty -> ident ty -> ident ty -> ident arrow_t

    val external_ :   ?label:annot -> unit -> ident ty

    val typedef_node : ?tyvs:ident typevar list -> type_def_visibility -> ident -> ident ty -> ident typedef_node
    val typedef :     ?label:annot -> ?tyvs:ident typevar list -> type_def_visibility -> ident -> ident ty -> ident typedef

    val typevar :                     ident -> ident typevar
    val var :         ?label:annot -> ident -> ident ty
    val fresh :       ?label:annot -> unit -> ident ty

    val coerce :      ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> ident ty -> (ident, 'a) expr
    val coerce_name : ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> string -> (ident, 'a) expr

    val record :      ?label:annot -> ?row:ident rowvar -> ident fields_t_node -> ident ty
    val tuple :       ?label:annot -> ?row:ident rowvar -> ident ty list -> ident ty

    val void :        ?label:annot -> unit -> ident ty
    val string :      ?label:annot -> unit -> ident ty
    val int :         ?label:annot -> unit -> ident ty
    val float :       ?label:annot -> unit -> ident ty
    val bool :        ?label:annot -> unit -> ident ty
  end

  (* Patterns *)
  (** @inline doc *)
  module P :
  sig
    val coerce :        ?label:annot -> ident pat -> ident ty -> ident pat
    val coerce_name :   ?label:annot -> ident pat -> string -> ident pat

    val any :           ?label:annot -> unit -> ident pat
    val ident :         ?label:annot -> ?directives:bind_directives list -> ident -> ident pat
    val var :           ?label:annot -> ?directives:bind_directives list -> ident -> ident pat

    val string :        ?label:annot -> string -> ident pat

    val record :        ?label:annot -> ?row:bool -> ident pat_record_node -> ident pat
    val record1   :     ?label:annot -> string -> ident pat -> ident pat
    val simple_record : ?label:annot -> string -> ident pat
    val tuple :         ?label:annot -> ident pat list -> ident pat
    val tuple_2 :       ?label:annot -> ident pat -> ident pat -> ident pat

    val void :          ?label:annot -> unit -> ident pat
    val true_ :         ?label:annot -> unit -> ident pat
    val false_ :        ?label:annot -> unit -> ident pat
    val bool :          ?label:annot -> bool -> ident pat
    val cons :          ?label:annot -> ident pat -> ident pat -> ident pat
    val nil :           ?label:annot -> unit -> ident pat
    val hd_tl :         ?label:annot -> ident pat -> ident pat -> ident pat
    val list :          ?label:annot -> ident pat list -> ident pat
    val none :          ?label:annot -> unit -> ident pat
    val some :          ?label:annot -> ident pat -> ident pat
  end

  (* Expressions *)
  (** @inline doc *)
  module E :
  sig
    val coerce :        ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> ident ty -> (ident, 'a) expr
    val coerce_name :   ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> string -> (ident, 'a) expr

    val record :        ?label:annot -> (ident, 'a) record_node -> (ident, 'a) expr
    val simple_record : ?label:annot -> string -> ('a, [< all_directives > `coerce ]) expr
    val record1 :       ?label:annot -> string -> (ident, 'a) expr -> (ident, 'a) expr
    val tuple :         ?label:annot -> (ident, 'a) expr list -> (ident, 'a) expr
    val tuple_2 :       ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr

    val constant :      ?label:annot -> const_expr_node -> (ident, _) expr
    val string :        ?label:annot -> string -> (ident, _) expr
    val float :         ?label:annot -> float -> (ident, _) expr
    val big_int :       ?label:annot -> Big_int.big_int -> (ident, _) expr
    val int :           ?label:annot -> int -> (ident, _) expr

    val ident :         ?label:annot -> ident -> (ident, _) expr
    val var :           ?label:annot -> ident -> (ident, _) expr

    val void :          ?label:annot -> unit -> ('a, [< all_directives > `coerce ]) expr
    val true_ :         ?label:annot -> unit -> (ident, [< all_directives > `coerce ]) expr
    val false_ :        ?label:annot -> unit -> (ident, [< all_directives > `coerce ]) expr
    val bool :          ?label:annot -> bool -> (ident, [< all_directives > `coerce ]) expr
    val cons :          ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr
    val nil :           ?label:annot -> unit -> (ident, [< all_directives > `coerce ]) expr
    val list :          ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr list -> (ident, 'a) expr
    val none :          ?label:annot -> unit -> (ident, [< all_directives > `coerce ]) expr
    val some :          ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr

    val dot :           ?label:annot -> (ident, 'a) expr -> string -> (ident, 'a) expr

    val lambda :        ?label:annot -> ident pat list -> (ident, 'a) expr -> (ident, 'a) expr
    val lambda_var :    ?label:annot -> ident -> (ident, 'a) expr -> (ident, 'a) expr
    val lambda_ignore : ?label:annot -> (ident, 'a) expr -> (ident, 'a) expr
    val lambda_void :   ?label:annot -> (ident, 'a) expr -> (ident, 'a) expr
    val applys :        ?label:annot -> (ident, 'a) expr -> (ident, 'a) expr list -> (ident, 'a) expr
    val apply :         ?label:annot -> (ident, 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr
    val apply2 :        ?label:annot -> (ident, 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr
    val apply_void :    ?label:annot -> (ident, ([> `coerce ] as 'a)) expr -> (ident, 'a) expr
    val eta_expand :    ?label:annot -> int -> (ident, 'a) expr -> (ident, 'a) expr

    val match_ :        ?label:annot -> (ident, 'a) expr -> (ident pat * (ident, 'a) expr) list -> (ident, 'a) expr
    val if_ :           ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr ->  (ident, 'a) expr -> (ident, 'a) expr
    val if_then :       ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr ->  (ident, 'a) expr
    val if_not :        ?label:annot -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr ->  (ident, 'a) expr
    val match_opt :     ?label:annot -> ?ty:ident ty -> (ident, [< all_directives > `coerce ] as 'a) expr -> ident pat * (ident, 'a) expr ->
      ident pat * (ident, 'a) expr -> (ident, 'a) expr
    val match_option :  ?label:annot -> ?ty:ident ty -> (ident, [< all_directives > `coerce ] as 'a) expr -> (ident, 'a) expr ->
      (ident -> (ident, 'a) expr) -> (ident, 'a) expr

    val letgen :        ?label:annot -> rec_:bool -> (ident * (ident, 'a) expr) list -> (ident, 'a) expr -> (ident, 'a) expr
    val letrec :        ?label:annot -> (ident * (ident, 'a) expr) list -> (ident, 'a) expr -> (ident, 'a) expr
    val letand :        ?label:annot -> (ident * (ident, 'a) expr) list -> (ident, 'a) expr -> (ident, 'a) expr
    val letin :         ?label:annot -> ident -> (ident, 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr
    val letins :        ?label:annot -> (ident * (ident, 'a) expr) list -> (ident, 'a) expr -> (ident, 'a) expr
    val bypass :        ?label:annot -> string -> (ident, 'a) expr
  end

  (* Directives *)
  (** @inline doc *)
  module D :
  sig
    (* directives types *)
    (** @inline doc *)
    module T :
    sig
      val static_source_content :    ?label:annot -> unit -> ident arrow_t
      val static_include_directory : ?label:annot -> unit -> ident arrow_t
      val static_binary_content :    ?label:annot -> unit -> ident arrow_t
      val assert_message :           ?label:annot -> unit -> ident arrow_t
      val ensure_message :           ?label:annot -> unit -> ident arrow_t
      val deprecated :               ?label:annot -> unit -> ident arrow_t
      val warning :                  ?label:annot -> unit -> ident arrow_t
      val assert_ :                  ?label:annot -> unit -> ident arrow_t
      val client :                   ?label:annot -> unit -> ident arrow_t
      val server :                   ?label:annot -> unit -> ident arrow_t
      val ensure :                   ?label:annot -> unit -> ident arrow_t
      val fail :                     ?label:annot -> unit -> ident arrow_t
      val force :                    ?label:annot -> unit -> ident arrow_t
      val private_ :                 ?label:annot -> unit -> ident arrow_t
      val protected :                ?label:annot -> unit -> ident arrow_t
      val slicer :                   ?label:annot -> unit -> ident arrow_t
      val translate :                ?label:annot -> unit -> ident arrow_t
      val unsafe_cast :              ?label:annot -> unit -> ident arrow_t
      val spawn :                    ?label:annot -> unit -> ident arrow_t
      val lazy_ :                    ?label:annot -> unit -> ident arrow_t
      val magic_to_string :          ?label:annot -> unit -> ident arrow_t
      val magic_to_xml :             ?label:annot -> unit -> ident arrow_t
      val side_annotation :          ?label:annot -> unit -> ident arrow_t
      val visibility_annotation :    ?label:annot -> unit -> ident arrow_t
      val i18n_lang :                ?label:annot -> unit -> ident arrow_t
    end
    (* directive expressions *)
    val open_ : ?label:annot -> (ident, [< all_directives > `open_ ] as 'a) expr -> (ident, 'a) expr -> (ident, 'a) expr
    val doctype : string list -> ?label:annot -> ?access:SurfaceAst.access_directive -> (ident, [< all_directives > `doctype ] as 'a) expr -> (ident, 'a) expr
    val string : ?label:annot -> (('a, [> `string ]) expr as 'expr) list -> 'expr
    val i18n_lang : ?label:annot -> unit -> ('a, [> `i18n_lang ]) expr

    val side_annotation : ?label:annot -> [`server | `client | `both | `prefer_server | `prefer_client | `prefer_both | `both_implem ] ->
                            (ident, [< all_directives > `side_annotation] as 'a) expr -> (ident,'a) expr
    val visibility_annotation : ?label:annot -> [`public of [`sync | `async | `funaction] | `private_] -> (ident, [< all_directives > `visibility_annotation] as 'a) expr -> (ident,'a) expr
    val static_content : ?label:annot -> ?factory_helper:(ident, 'a) expr -> bool -> string -> (ident, [> `static_content of (string * bool) ] as 'a) expr
    val static_resource: ?label:annot -> ?factory_helper:(ident, 'a) expr -> string -> (ident, [> `static_resource of string ] as 'a) expr
    val server_entry_point : ?label:annot -> (ident, [> `server_entry_point ] as
  'a) expr  -> (ident, 'a) expr
    val with_thread_context : ?label:annot -> (ident, [> `with_thread_context]
      as 'a)  expr -> (ident, 'a)  expr -> (ident, 'a) expr
  end

  (* code element *)
  (** @inline doc *)
  module C :
  sig
    val newval_pel :    ?rec_:bool -> ?label:annot -> (ident pat * (ident, 'a) expr) list -> (ident, 'a) code_elt
    val newval :        ?label:annot -> ident -> (ident, 'a) expr -> (ident, 'a) code_elt
    val newvalrec :     ?label:annot -> ident -> (ident, 'a) expr -> (ident, 'a) code_elt
    val newval_ignore : ?label:annot -> (ident, 'a) expr -> (ident, 'a) code_elt
    val newtype :       ?label:annot -> ident typedef -> (ident, _) code_elt
  end

end
