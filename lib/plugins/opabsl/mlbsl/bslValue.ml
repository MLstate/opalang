(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(** Provides some functions for manipulate runtime values on Opa. *)

##property[mli]
(** Type of field of records. *)
##extern-type Record.field = ServerLib.field

(** Type of record constructor. *)
##extern-type Record.constructor = ServerLib.record_constructor

(** Type of record patterns indexes. *)
##extern-type Record.patterns_indexes = ServerLib.patterns_indexes

(** Type of record fields indexes. *)
##extern-type Record.fields_indexes = ServerLib.fields_indexes

(** Type of record field index. *)
##extern-type Record.field_index = ServerLib.field_index
##property[endmli]

(** A module for manipulate and construct records. It's just alias to
      some functions of the [ServerLib].
      @see <./BslServerLib.S.html> Interface of the server lib
*)
##module record

  ##register dot : 'a, Record.field -> option('b)
  let dot r f = ServerLib.dot (Obj.magic r) f

  ##register unsafe_dot : 'a, Record.field -> 'b
  let unsafe_dot r f = ServerLib.unsafe_dot (Obj.magic r) f

  ##register fold_record : (Record.field, 'a, 'b -> 'b), 'c, 'b -> 'b
  let fold_record = ServerLib.fold_record

  ##register fold_2_record : (Record.field, 'a, 'a, 'b -> 'b), 'c, 'c, 'b -> 'b
  let fold_2_record = ServerLib.fold_2_record

  ##register name_of_field : Record.field -> option(string)
  let name_of_field = ServerLib.name_of_field

  ##register field_of_name : string -> option(Record.field)
  let field_of_name = ServerLib.field_of_name

  (** The empty record constructor. *)
  ##register empty_constructor : -> Record.constructor
  let empty_constructor _ = ServerLib.empty_record_constructor

  ##register add_field : Record.constructor, Record.field, 'c -> Record.constructor
  let add_field = ServerLib.add_field

  ##register make_record : Record.constructor -> 'c
  let make_record c = Obj.magic (ServerLib.make_record c)

  ##register make_simple_record : Record.field -> _
  let make_simple_record field =
    Obj.magic (ServerLib.make_simple_record field)

  ##register fields_indexes : llarray(Record.field) -> Record.fields_indexes
  let fields_indexes fields = ServerLib.fields_indexes (Obj.magic (fields:Obj.t array): ServerLib.field array)

  ##register field_index          \ `ServerLib.field_index`         : Record.fields_indexes,Record.field -> Record.field_index

  ##register dot_with_field_index : 'record,Record.field_index -> 'field_content
  let dot_with_field_index record field_index = ServerLib.dot_with_field_index (Obj.magic record:ServerLib.ty_record) field_index

  ##register patterns_indexes : llarray(Record.fields_indexes) -> Record.patterns_indexes
  let patterns_indexes patterns = ServerLib.patterns_indexes (Obj.magic (patterns:Obj.t array): ServerLib.fields_indexes array)

  ##register compare_structure    \ `ServerLib.compare_structure`   : Record.patterns_indexes,'record,'record -> int
  let compare_structure pi r1 r2 = ServerLib.compare_structure pi (Obj.magic r1:ServerLib.ty_record) (Obj.magic r2:ServerLib.ty_record)

##endmodule


(** This module is very dangerous, don't use it directly. It's a
    module for explicit instantiation. It allow to associated a string
    with type scheme.*)
##module tsc

  (** The association table. *)
  let tsctbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 1024

  (** Register a type scheme. *)
  ##register [opacapi] add : string, 'c -> void
  let add name tsc =
    Hashtbl.add tsctbl name (Obj.repr tsc)

  (** Get the type scheme as an option. *)
  ##register get : string -> option('c)
  let get name =
    try
      Some (Obj.obj (Hashtbl.find tsctbl name))
    with Not_found -> None

##endmodule


(** Used for register and get some specialized function for magic
    function. *)
##module MagicContainer

  let to_string_tbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 16

  ##register to_string_add : string, 'a -> void
  let to_string_add k o = Hashtbl.add to_string_tbl k (Obj.repr o)

  ##register to_string_get : string -> option('a)
  let to_string_get k =
    try
      Some (Obj.obj (Hashtbl.find to_string_tbl k))
    with Not_found -> None

  let compare_tbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 16

  ##register compare_add : string, 'a -> void
  let compare_add k o = Hashtbl.add compare_tbl k (Obj.repr o)

  ##register compare_get : string -> option('a)
  let compare_get k =
    try
      Some (Obj.obj (Hashtbl.find compare_tbl k))
    with Not_found -> None

  let serializer_tbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 16

  ##register serializer_add : string, 'a -> void
  let serializer_add k o = Hashtbl.add serializer_tbl k (Obj.repr o)

  ##register serializer_get : string -> option('a)
  let serializer_get k =
    try
      Some (Obj.obj (Hashtbl.find serializer_tbl k))
    with Not_found -> None

  let xmlizer_tbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 16

  ##register xmlizer_add : string, 'a -> void
  let xmlizer_add k o = Hashtbl.add xmlizer_tbl k (Obj.repr o)

  ##register xmlizer_get : string -> option('a)
  let xmlizer_get k =
    try
      Some (Obj.obj (Hashtbl.find xmlizer_tbl k))
    with Not_found -> None

##endmodule
