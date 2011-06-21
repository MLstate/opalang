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

(**)

module Remove :
  sig
    type ('a,'b) through
    module Basic :
      sig
        val access_directive:('a, [< all_directives > `private_ `package `public ]) through
        val access_not_public:('a, [< all_directives > `private_ `package `public ]) through
        val coerce :         ('a, [< all_directives > `coerce ]) through
        val deprecated :         ('a, [< all_directives > `deprecated ]) through
        val directive :      ('a, 'b) through
        val doctype :        ('a, [< all_directives > `doctype ]) through
        val expand :         ('a, [< all_directives > `expand  ]) through
        val magic_directive : ('a, [> magic_directive]) through
        val lambda :         ('a, 'b) through
        val letin :          ('a, 'b) through
        val opacapi :         ('a, [< all_directives > `opacapi  ]) through
        val opavalue_directive : ('a, [> opavalue_directive]) through
        val open_ :          ('a, [< all_directives > `open_ ]) through
        val private_ :       ('a, [< all_directives > `private_ ]) through
        val slicer_directive:('a, [< all_directives > `side_annotation `visibility_annotation ]) through
        val side_annotation :('a, [< all_directives > `side_annotation ]) through
        val visibility_annotation:('a, [< all_directives > `visibility_annotation ]) through
      end
    val remove : through:('a, [< all_directives ] as 'b) through list -> ('a, 'b) expr -> ('a, 'b) expr
    val coerce : ('a, [< all_directives > `coerce] as 'b) expr -> ('a, 'b) expr
  end


module Look :
  sig
    type ('a,'b) through = ('a,'b) Remove.through
    val apply :       ?through:('a,'b) through list -> ('a, 'b) expr -> bool
    val lambda :      ?through:('a,'b) through list -> ('a, 'b) expr -> bool
    val record :      ?through:('a,'b) through list -> ('a, 'b) expr -> bool
    val module_ :     ?through:('a, [< all_directives > `module_ ] as 'b) through list -> ('a, 'b) expr -> bool
    val module_local :?through:('a, [< all_directives > `module_ `local ] as 'b) through list -> ('a, 'b) expr -> bool
    val private_ : ?through:('a, [< all_directives > `private_ ] as 'b) through list -> ('a, 'b) expr -> bool

    val at : ?through:('a,'b) through list -> at:('a,'b) through list -> ('a, 'b) expr -> bool
    (** [at ~through:patterns ~at:patterns e] returns true when [e] matches one of the patterns of [at]
        (while ignoring [through])
    *)
  end

(* should be renamed GetThrough *)
module FoldThrough :
  sig
    val dot :   ?through:('a, 'b) Remove.through list -> ('a, 'b) expr -> ('a, 'b) expr * (string * QmlLoc.annot) list
    val arity : ?through:('a, 'b) Remove.through list -> ('a, 'b) expr -> int option
    val fields : ?through:('a, ([> `module_ ] as 'b)) Remove.through list -> ('a, 'b) expr -> ('a, 'b) record_node option
    (** gets the fields of records or modules *)
  end


module Context :
  sig
    type ('ident,'input,'output) through_with_context
    module Basic :
      sig
        val coerce :       ('a, [< all_directives > `coerce ], [< all_directives > `coerce ]) through_with_context
        val directive :    ('a,'b,'b) through_with_context
        val letin :        ('a,'b,'b) through_with_context
        val opacapi :        ('a,[< all_directives > `opacapi ] as 'b,'b) through_with_context
        val opavalue_directive :
          ('a, [> opavalue_directive ] as 'b, 'b) through_with_context
        val open_ :        ('a,[< all_directives > `open_ ] as 'b,'b) through_with_context
        val doctype :      ('a, [< all_directives > `doctype ] as 'b, 'b) through_with_context
        val slicer_directive :
          ('a, [< all_directives > `side_annotation `visibility_annotation] as 'b, 'b)
          through_with_context
        val side_annotation :
          ('a, [< all_directives > `side_annotation] as 'b, 'b)
          through_with_context
        val visibility_annotation :
          ('a, [< all_directives > `visibility_annotation] as 'b, 'b)
          through_with_context

      end
    val remove : through:('ident, 'input, 'output) through_with_context list ->
           ('ident, 'input) expr ->
           ('ident, 'input) expr * (('ident, 'output) expr -> ('ident, 'output) expr)
    val filter : keep:('ident, 'input, 'output) through_with_context list ->
                 throw:('ident,'input) Remove.through list ->
                 ('ident, 'input) expr ->
                 ('ident, 'input) expr * (('ident, 'output) expr -> ('ident, 'output) expr)
    val filter2 : keep1:('ident, 'input, 'output) through_with_context list ->
                  keep2:('ident, 'input, 'output2) through_with_context list ->
                  throw:('ident,'input) Remove.through list ->
                  ('ident, 'input) expr ->
                  ('ident, 'input) expr *
                    (('ident, 'output) expr -> ('ident, 'output) expr) *
                    (('ident, 'output2) expr -> ('ident, 'output2) expr)
    val uncoerce : ('a,[< all_directives > `coerce] as 'b) expr ->
                   ('a, 'b) expr * (('a, [< all_directives > `coerce ] as 'c) expr -> ('a, 'c) expr)
    val unletin :  ('a,'b) expr ->
                   ('a,'b) expr * (('a,'b) expr -> ('a,'b) expr)
  end


module FoldContext :
  sig
    val letin : ?through:('a,'b,'c) Context.through_with_context list ->
         ('a, 'b) expr -> ('a, 'b) expr * ('a * ('a, 'b) expr) list * (('a,'c) expr -> ('a,'c) expr)
  end
