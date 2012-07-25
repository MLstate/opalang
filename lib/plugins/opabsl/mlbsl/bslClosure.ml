(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
##extern-type Closure.args = QmlClosureRuntime.AnyArray.t
##extern-type Closure.t = QmlClosureRuntime.t
##opa-type Closure.info
##module Args
  ##register create \ `QmlClosureRuntime.AnyArray.create` : int -> Closure.args
  ##register length \ `QmlClosureRuntime.AnyArray.length` : Closure.args -> int
  ##register set \ `QmlClosureRuntime.AnyArray.set` : Closure.args, int, 'a -> void
  ##register get \ `QmlClosureRuntime.AnyArray.get` : Closure.args, int -> 'a
##endmodule

##register show \ `QmlClosureRuntime.show` : Closure.t -> string
##register create \ `QmlClosureRuntime.create` : 'impl, int, Closure.info -> Closure.t
##register create_no_function \ `QmlClosureRuntime.create_no_function` : int, Closure.info -> Closure.t
##register [opacapi] define_function \ `QmlClosureRuntime.define_function` : Closure.t, 'impl -> void
##register apply \ `QmlClosureRuntime.args_apply` : Closure.t, Closure.args -> 'a
##register env_apply_with_ty \ `QmlClosureRuntime.env_apply_with_ty` : Closure.t, Closure.args,Closure.args -> Closure.t

##register is_empty \ `QmlClosureRuntime.is_empty` : Closure.t -> bool
##register get_info \ `QmlClosureRuntime.get_identifier` : 'closure -> option(Closure.info)
##register set_info \ `QmlClosureRuntime.set_identifier` : Closure.t, Closure.info -> void
##register get_args \ `QmlClosureRuntime.get_args` : Closure.t -> Closure.args
##register get_ty_args \ `QmlClosureRuntime.get_tyargs` : Closure.t -> Closure.args
##register export \ `QmlClosureRuntime.export` : Closure.t -> 'a
##register import \ `QmlClosureRuntime.import` : 'a, int -> Closure.t

(** Create an "anyarray" closure. These closures consider that the
    given native implementation is a function that takes an anyarray
    of size n rather than a function that take n arguments.
    see also [opavalue.opa (OpaValue.Closure)]
*)
##register create_anyarray \ `QmlClosureRuntime.create_anyarray` : 'impl, int, Closure.info -> Closure.t

(** Create an "anyarray" closure with cps code transformation.
    i.e. We suppose that the given impl is a native function
    typed by [Closure.args -> 'a cont -> unit].
    see also [opavalue.opa (OpaValue.Closure)]
*)
##register [cps-bypass] create_anyarray_cps : 'impl, int, Closure.info, continuation(Closure.t) -> void
let create_anyarray_cps func arity ident k =
  let any_cps oargs =
    let args = QmlClosureRuntime.AnyArray.sub2 oargs 0 arity in
    let k = QmlClosureRuntime.AnyArray.get oargs arity in
    ((Obj.magic func) : _ (*args*) -> _ (*cont*) -> _) args k
  in
  (* arity+1 for continuation added by cps *)
  let cl = QmlClosureRuntime.create_anyarray any_cps (arity + 1) ident in
  QmlCpsServerLib.return k cl

##register [cps-bypass] apply_cps : Closure.t, Closure.args, continuation('a) -> void
let apply_cps closure args k =
  (* Add k only on full application *)
  assert (
    if (
      closure.QmlClosureRuntime.arity - 1 =
        (QmlClosureRuntime.AnyArray.length closure.QmlClosureRuntime.args)
        + (QmlClosureRuntime.AnyArray.length args)
    ) then true else (
      Printf.printf "BslClosure.apply_cps:\n  closure:%s\n  args:%s\n  k:%s\n"
        (DebugPrint.print closure)
        (DebugPrint.print args)
        (DebugPrint.print k);
      false
    )
  );
  let args = QmlClosureRuntime.AnyArray.append args [|Obj.repr k|] in
  QmlClosureRuntime.args_apply closure args


(** A function used to print closure-related debug info*)
##register closure_debug_from_opa: string, string -> void
let closure_debug_from_opa __topic __msg =
  #<If:CLOSURE_DEBUG>
    Logger.debug "[CLOSURE][%s] %s %!" __topic __msg
  #<Else>
    ()
  #<End>



(* CLOSURE REGISTERING *)
(** Type of the information. *)
type t = {
  mutable local : QmlClosureRuntime.t;
  mutable distant : bool;
}

(** The association table. *)
let funtbl : (string, t) Hashtbl.t = Hashtbl.create 1024

let set str t = Hashtbl.add funtbl str t


##register on_distant : string -> bool
(** [on_distant "toto"] If returns true the "toto" function is
    present on the other side. *)
let on_distant str =
  Hashtbl.mem funtbl str && (Hashtbl.find funtbl str).distant

let fclosure_name = ServerLib.static_field_of_name "closure_name"

let make_closure_name name =
  ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor fclosure_name (ServerLib.wrap_string name))

##register [opacapi] create_and_register : 'impl, int, string, bool -> Closure.t
let create_and_register impl arity name distant =
  let ident = make_closure_name name in
  let closure = QmlClosureRuntime.create impl arity ident in
  set name { local = closure; distant = distant };
  closure

##register [opacapi] create_no_function_and_register : int, string, bool -> Closure.t
let create_no_function_and_register arity name distant =
  let ident = make_closure_name name in
  let closure = QmlClosureRuntime.create_no_function arity ident in
  set name { local = closure; distant = distant };
  closure

##register get_local : string -> option(Closure.t)
(** Get the local function, if returns [None] functions is not
    present locally. *)
let get_local str =
  try
    Some (Hashtbl.find funtbl str).local
  with Not_found -> None

##register [opacapi] set_distant : string, bool -> void
(** If a client function get cleaned, set its distant property to
    false.  No need to create a container if the function was not
    already known. *)
let set_distant str b =
  try
    (Hashtbl.find funtbl str).distant <- b;
  with Not_found ->
    ()

##register replace_identifier: string, string -> void
let replace_identifier key_ident ident =
  try
    let {distant; local=clos} = Hashtbl.find funtbl key_ident in
    assert (clos.QmlClosureRuntime.identifier <> None);
    clos.QmlClosureRuntime.identifier <- Some (Obj.repr (make_closure_name ident));
    Hashtbl.remove funtbl key_ident;
    Hashtbl.add funtbl ident {distant; local=clos};
   #<If:JS_RENAMING>
     Printf.printf "Closure update: %s to %s\n%!" key_ident ident;
   #<End>
  with Not_found ->
    #<If:JS_RENAMING>
      Printf.printf "Closure NO update: %s to %s\n%!" key_ident ident;
    #<End>;
    ()
