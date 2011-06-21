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
(* this is a sample to show how use opa with a bypass plugin *)
##register myfunction : string -> string
let myfunction s =
   let len = String.length s in
   let s' = String.copy s in
    for i = 0 to pred len do
      s'.[i] <- s.[len-i-1]
    done;
    s'

##extern-type 'a toto = 'a
external identity : 'a -> 'a = "%identity"
##register wrap\identity : 'a -> toto('a)
##register unwrap\identity : toto('a) -> 'a

##register dbl314 : float -> float
let dbl314 x = (2. *. 3.14) *. x

##module Proj

##register string : -> opa[string]
let string () = ServerLib.wrap_string "toto"

##register option : 'a -> opa[option('a)]
let option a = ServerLib.wrap_option (Some a)

##endmodule

##opa-type intlist
##opa-type list('a)
##extern-type 'a ocaml_list = 'a list

let (+++) (f, x) r =
  ServerLib.add_field r
    (ServerLib.static_field_of_name f)
    (Obj.magic x)
let (|+>) r (f, x) = (f, x) +++ r
let (!!!) r = ServerLib.make_record r
let zero = ServerLib.empty_record_constructor

##register mk_void : void -> opa[void]
let mk_void () = ServerLib.void

let void = ServerLib.void

##register mk_nil : void -> opa[list('a)]
let mk_nil () =
  let mk_nil0 () = ("nil", void) +++ zero in
  let record = !!! (mk_nil0 ()) in
  wrap_opa_list record

##register mk_cons : 'a, opa[list('a)] -> opa[list('a)]
let mk_cons hd tl =
  let mk_cons0 hd tl = ("hd", hd) +++ (("tl", tl) +++ zero) in
  let record = !!! (mk_cons0 hd tl) in
  wrap_opa_list record

##register opa_list_of_ocaml_list : ('a -> 'b), ocaml_list('a) -> opa[list('b)]
let opa_list_of_ocaml_list alphaproj list =
  List.fold_right (fun alpha acc ->
    let alpha = alphaproj alpha in
    mk_cons alpha acc) list (mk_nil ())

##register init : int -> opa[list(int)]
let init i =
  let list = Base.List.init i (fun i -> i) in
  opa_list_of_ocaml_list (fun i -> i) list

##register marshal : 'a -> string
let marshal a = Marshal.to_string a []

##register unmarshal : string -> 'a
let unmarshal s =
  let a = Marshal.from_string s 0 in
  ServerLib.sharing_refresh a

##module Modale

##opa-type [opaname] Toto.tutu

##register tutu : opa[Toto.tutu] -> void
let tutu _ = ()

##endmodule
