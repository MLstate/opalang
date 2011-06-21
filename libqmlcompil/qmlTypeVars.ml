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
(*
  Authors: 2006 - 2009, Henri Binsztok    <Henri.Binsztok@mlstate.com>
           2009, Vincent Benayoun         <Vincent.Benayoun@mlstate.com>
           2009, Mikolaj Konarski         <Mikolaj.Konarski@mlstate.com>
           2009, Mathieu Barbin           <Mathieu.Barbin@mlstate.com>
           2009, Louis Gesbert            <Louis.Gesbert@mlstate.com>
           2009, Mehdi Bouaziz            <Mehdi.Bouaziz@mlstate.com>
           2009, David Rajchenbach-Teller <David.Teller@mlstate.com>
*)

(**)

module type GEN_VAR = Fresh.FRESH

module MakeVar ( FB : Fresh.BRAND ) : GEN_VAR =
  Fresh.FreshGen ( FB )

let var_printer s =
  fun (_, id, n, d) -> Printf.sprintf "'%s%d%s%s%s" s id n (if d="" then "" else "_") d

module TypeVar =
  MakeVar (struct let printer = var_printer "v" end)

module RowVar =
  MakeVar (struct let printer = var_printer "r" end)

module ColVar =
  MakeVar (struct let printer = var_printer "c" end)


module TypeVarMap : (BaseMapSig.S with type key = TypeVar.t)
  = BaseMap.Make ( TypeVar )
module TypeVarSet : (BaseSetSig.S with type elt = TypeVar.t)
  = BaseSet.Make ( TypeVar )
module TypeVarSetMap
  = SetMap.Make ( TypeVar ) ( TypeVar )
module TypeVarTbl
  = Base.Hashtbl.Make ( TypeVar )

module RowVarMap : (BaseMapSig.S with type key = RowVar.t)
  = BaseMap.Make ( RowVar )
module RowVarSet : (BaseSetSig.S with type elt = RowVar.t)
  = BaseSet.Make ( RowVar )
module RowVarSetMap
  = SetMap.Make ( RowVar ) ( RowVar )
module RowVarTbl
  = Base.Hashtbl.Make ( RowVar )

module ColVarMap : (BaseMapSig.S with type key = ColVar.t)
  = BaseMap.Make ( ColVar )
module ColVarSet : (BaseSetSig.S with type elt = ColVar.t)
  = BaseSet.Make ( ColVar )
module ColVarSetMap
  = SetMap.Make ( ColVar ) ( ColVar )
module ColVarTbl
  = Base.Hashtbl.Make ( ColVar )

type ('a, 'b, 'c) generic_quantif =
    {
      typevar : 'a ;
      rowvar  : 'b ;
      colvar  : 'c ;
    }
type quantif =
    (TypeVarSet.t, RowVarSet.t, ColVarSet.t) generic_quantif


module FreeVars =
struct
  type t = quantif

  let empty =
    (
     {
       typevar = TypeVarSet.empty;
       rowvar = RowVarSet.empty;
       colvar = ColVarSet.empty
     } : t
    )
  let is_type_empty f = TypeVarSet.is_empty f.typevar
  let is_row_empty f = RowVarSet.is_empty f.rowvar
  let is_col_empty f = ColVarSet.is_empty f.colvar
  let is_empty f = is_type_empty f && is_row_empty f && is_col_empty f

  let compare f1 f2 =
    let cmp = TypeVarSet.compare f1.typevar f2.typevar in
    if cmp <> 0 then cmp
    else
      let cmp = RowVarSet.compare f1.rowvar f2.rowvar in
      if cmp <> 0 then cmp
      else ColVarSet.compare f1.colvar f2.colvar

  let union a b =
    {
      typevar = TypeVarSet.union a.typevar b.typevar;
      rowvar = RowVarSet.union a.rowvar b.rowvar;
      colvar = ColVarSet.union a.colvar b.colvar
    }
  let diff a b =
    {
      typevar = TypeVarSet.diff a.typevar b.typevar;
      rowvar = RowVarSet.diff a.rowvar b.rowvar;
      colvar = ColVarSet.diff a.colvar b.colvar
    }
  let inter a b =
    {
      typevar = TypeVarSet.inter a.typevar b.typevar;
      rowvar = RowVarSet.inter a.rowvar b.rowvar;
      colvar = ColVarSet.inter a.colvar b.colvar
    }
  let subset a b =
    TypeVarSet.subset a.typevar b.typevar &&
    RowVarSet.subset a.rowvar b.rowvar &&
    ColVarSet.subset a.colvar b.colvar
  let equal a b = a == b ||
    (* TODO: OPTIMIZE: FRAGILE: this should work, too: a = b;
       but it's not worth the danger, until it's used outside assertions *)
    TypeVarSet.equal a.typevar b.typevar &&
    RowVarSet.equal a.rowvar b.rowvar &&
    ColVarSet.equal a.colvar b.colvar

  let add_ty v free = { free with typevar = TypeVarSet.add v free.typevar }
  let add_row v free = { free with rowvar = RowVarSet.add v free.rowvar }
  let add_col v free = { free with colvar = ColVarSet.add v free.colvar }

  let map f_tv f_rv f_cv f =
    {
      typevar = TypeVarSet.map f_tv f.typevar;
      rowvar = RowVarSet.map f_rv f.rowvar;
      colvar = ColVarSet.map f_cv f.colvar
    }

  let mem_typevar v f = TypeVarSet.mem v f.typevar
  let mem_rowvar rv f = RowVarSet.mem rv f.rowvar
  let mem_colvar cv f = ColVarSet.mem cv f.colvar

  let remove_ty v free = { free with typevar = TypeVarSet.remove v free.typevar }
  let remove_row v free = { free with rowvar = RowVarSet.remove v free.rowvar }
  let remove_col v free = { free with colvar = ColVarSet.remove v free.colvar }

  let refresh f = map TypeVar.refresh RowVar.refresh ColVar.refresh f

  let export_as_lists f =
    (TypeVarSet.elements f.typevar,
     RowVarSet.elements f.rowvar,
     ColVarSet.elements f.colvar)

  let import_from_sets typevar rowvar colvar =
    {
      typevar = typevar;
      rowvar = rowvar;
      colvar = colvar
    }

  let add_list (vars,rows,cols) {typevar; rowvar; colvar} = {
    typevar = TypeVarSet.add_list vars typevar;
    rowvar = RowVarSet.add_list rows rowvar;
    colvar = ColVarSet.add_list cols colvar;
  }

  let typevar = TypeVar.to_string
  and rowvar = RowVar.to_string
  and colvar = ColVar.to_string

  let to_string {typevar = typevars; rowvar = rowvars; colvar = colvars} =
    let typevars = TypeVarSet.elements typevars in
    let rowvars = RowVarSet.elements rowvars in
    let colvars = ColVarSet.elements colvars in
    Printf.sprintf "([%s], [row: %s], [col: %s])"
      (BaseString.concat_map "; " typevar typevars)
      (BaseString.concat_map "; " rowvar rowvars)
      (BaseString.concat_map "; " colvar colvars)
end

(** {6 Pretty Printing} *)

(**
   Guidelines, design of printing for Type Variables :

   You can build a new scope each time you want to reset the pretty-printing.

   The rule is so :

   Whenever it is possible, the output chosen for a variable is its original name
   in the source. In case of conflict, we fallback to 'v%d
*)

module type VAR_PRINT =
sig
  type t
  type scope
  val new_scope : unit -> scope

  (**
     Reste the scope to its initial state
  *)
  val reset : scope -> unit

  (**
     Push a new block in the scope
  *)
  val push : scope -> unit

  (**
     Pop the top block of the scope
  *)
  val pop : scope -> unit

  val pp : scope -> Format.formatter -> t -> unit
  val get : scope -> t -> string
end

let pretty_var i = "'v" ^ (string_of_int i)

module MakeVarPrint (Var : GEN_VAR) (VarMap : BaseMapSig.S with type key = Var.t) : VAR_PRINT with type t = Var.t =
struct
  type t = Var.t

  module VarScope = ImperativeScope.Default(struct type elt = Var.t end)
  module NameScope = ImperativeScope.Default(struct type elt = string end)

  type scope = {
    vars : string VarScope.t ;
    names : Var.t NameScope.t ;
  }

  let reset scope =
    VarScope.reset scope.vars ;
    NameScope.reset scope.names ;
    ()

  let new_scope () = {
    vars = VarScope.create 16 ;
    names = NameScope.create 16 ;
  }

  let push scope =
    VarScope.push scope.vars ;
    NameScope.push scope.names ;
    ()

  let pop scope =
    VarScope.pop scope.vars ;
    NameScope.pop scope.names ;
    ()

  (* FIXME : remove the hack, the name should never contains '
     it is added only for printing,
     the get function should return only the contains of the ident,
     without the '
     ( :( c'est pas maintenant )
  *)

  (*
    find next, using a closure generating a proposition of name from an int
  *)
  let find_next_free_name names next =
    let rec aux i =
      let name = next i in
      match NameScope.find_opt names name with
      | None -> name
      | Some _ -> aux (succ i)
    in
    aux 0

  let get scope t =
    match VarScope.find_opt scope.vars t with
    | Some name -> name
    | None ->
        (*
          2 cases: the variable has a name, in that case, we will try to use it,
          or if not, we will use the pretty_var version
        *)
        let next =
          let name = Var.name t in
          if name = ""
          then
            pretty_var
          else
            (* hack: for some weird reasons, some names have already a ' in their names *)
            let name = if name.[0] <> '\'' then "'"^name else name in
            (function 0 -> name | i -> name ^ (string_of_int (succ i)))
        in
        let name = find_next_free_name scope.names next in
        VarScope.bind scope.vars t name ;
        NameScope.bind scope.names name t ;
        name

  let pp scope fmt t =
    let string = get scope t in
    Format.pp_print_string fmt string
end

module TypeVarPrint = MakeVarPrint ( TypeVar ) ( TypeVarMap )
module RowVarPrint = MakeVarPrint ( RowVar ) ( RowVarMap )
module ColVarPrint = MakeVarPrint ( ColVar ) ( ColVarMap )

(*
 * [get_canonical_typevar n] returns a type variable
 * (always the same for a given index)
 * Type variables are generated in decreasing order
 *
 * Useful for building canonical representation of types
 *)
module CanonicalVarGenerator(M : GEN_VAR) =
struct
  let gen =
    let h = Hashtbl.create 10 in
    Hashtbl.add h (-1) (M.prev ()); (* making sure that -1 maps to "v-1" *)
    let max = ref (-1) in
    fun n ->
      assert (n >= -1);
      try Hashtbl.find h n
      with Not_found ->
        for i = !max + 1 to n do (* if someone asks for 1 and 3, you must generate
                                  * 2 anyway to be sure that it is greater that 3 *)
          Hashtbl.add h i (M.prev ())
        done;
        max := Pervasives.max !max n;
        Hashtbl.find h n

(* filling the hashtbl below at toplevel so that we are the first one to generate
 * type variables this way to be sure, that every time you run the compiler
 * you have the same type variables (important for sharing across compilation unit
 * of types from ei in js) *)
  let () =
    for i = 0 to 100 do
      ignore (gen i)
    done
end

module CanonicalVar = CanonicalVarGenerator (TypeVar)
module CanonicalRow = CanonicalVarGenerator (RowVar)
module CanonicalCol = CanonicalVarGenerator (ColVar)
let get_canonical_typevar : int -> TypeVar.t = CanonicalVar.gen
