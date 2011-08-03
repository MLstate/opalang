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
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module Cons = O.Cons
module G = Grammar
module T = Tools
module L = B.List
module A = B.Array
module S = B.String

let tostr v ts_opt t_opt =
  match ts_opt, t_opt with
  | (Some (_,ts),_) -> "("^ts^" "^v^")"
  | (_,None) -> v
  | (_,Some (O.TypeConst O.TypeUnit)) -> "((fun () -> \"()\") "^v^")"
  | (_,Some (O.TypeConst O.TypeBool)) -> "(string_of_bool "^v^")"
  | (_,Some (O.TypeConst O.TypeInt)) -> "(string_of_int "^v^")"
  | (_,Some (O.TypeConst O.TypeInt64)) -> "(Int64.to_string "^v^")"
  | (_,Some (O.TypeName (_,["int64"]))) -> "(Int64.to_string "^v^")"
  | (_,Some (O.TypeConst O.TypeFloat)) -> "(string_of_float "^v^")"
  | (_,Some (O.TypeConst O.TypeString)) -> v
  | (_,Some (O.TypeName ([O.TypeConst O.TypeString], ["list"]))) -> "(String.concat \" \" "^v^")"
  | (_,Some t) ->
      OcamlPrint.Output.type_expr stderr t; prerr_newline (); flush stderr;
      failwith "Can't convert such a type to string."

let cats s1 s2 =
  match (s1,s2) with
  | s1,"" -> s1
  | s1,"\"\"" -> s1
  | "",s2 -> s2
  | "\"\"",s2 -> s2
  | s1,s2 -> s1^"^"^s2

let pattern_of_raw = function
  | G.Raw (name, (pre1,opts), lst) ->
      let vlist = L.map (fun (v,_,_,_) -> v) lst in
      let pvlist = L.map (fun v -> O.PatVar (Ident.source v)) vlist in
      let dostr = function Some (l,_) -> l | None -> "" in
      let verbatim = cats pre1 (L.fold_left (fun s (v,t_opt,l_opt,ts_opt) ->
                                                  cats (cats s (tostr v ts_opt t_opt)) (dostr l_opt)) "" lst) in
      O.PatConstructor ([Ident.source name], pvlist), O.Verbatim verbatim
  | _ -> assert false

(* Some conversion functions. *)
let do_it lst =
  let tuple_list = L.map pattern_of_raw lst in
  let func = O.Function (L.map (fun (a, b) -> a, None, b) tuple_list) in
    O.Let [O.Pat (O.PatVar (Ident.source "string_of_rawmsg")), func]
