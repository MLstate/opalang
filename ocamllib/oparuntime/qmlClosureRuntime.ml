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
#<Debugvar:CLOSURE_DEBUG>

(*-----------------------------*)
(*--- a few datastructures  ---*)
(*-----------------------------*)

type 'a tuple1 = {tuple1 : 'a}
    (* the fake type that i use to replace the missing 1-uple
     * in the syntax *)

(* this module is meant to allow heteregeneous arrays
 * and carefully avoids troubles with ocaml optimization
 * of float arrays *)
module AnyArray : sig
  (* beware : values of this type are created
   * with Obj.magic in the generated code *)
  type t = Obj.t array
  val empty : t
  val create : int -> t
  val set : t -> int -> 'a -> unit
  val get : t -> int -> 'a
  val length : t -> int
  val append_sub : t -> int -> t -> t
  val sub : t -> int -> t
  val sub2 : t -> int -> int -> t
  val append : t -> t -> t
end =
struct
  type t = Obj.t array
  let empty = [||]
  let create n = Array.make n (Obj.repr 0)
  let set a i x = a.(i) <- Obj.repr x
  let get a i = Obj.obj a.(i)
  let length = Array.length
  let append_sub a i b =
    let n = Array.length a
    and m = Array.length b in
    let c = create (n+m-i) in
    Array.blit a 0 c 0 n;
    Array.blit b i c n (m-i);
    c
  let sub a n =
    let b = create n in
    Array.blit a 0 b 0 n;
    b
  let append a b = append_sub a 0 b
  let sub2 a i j =
    let b = create (j-i) in
    Array.blit a i b 0 (j-i);
    b
end

(* FIXME: could we avoid the overhead of transforming lists in arrays ? *)
(* heterogenous lists *)
module AnyList : sig
  type t
  val empty : t
  val is_empty : t -> bool
  val push : t -> 'a -> t (* add at the end of the list *)
  val length : t -> int
  val to_anyarray : t -> AnyArray.t
end =
struct
  (* perhaps it is less efficient to keep the length rather than computing it in to_any_array *)
  type t = Obj.t list * int (* the list is reversed *)
  let empty = ([],0)
  let is_empty = function
    | (_,0) -> true
    | _ -> false
  let push (l,n) elt = (Obj.repr elt :: l, n+1)
  let length = snd
  let to_anyarray (l,n) =
    let array = AnyArray.create n in
    Base.List.iteri (fun x i -> AnyArray.set array (n-1-i) x) l;
    array
end

#<Ifstatic:CPS_WITH_ML_CLOSURE .*>

(*------------------------*)
(*----- fake implem ------*)
(*------------------------*)
(* this is meant to allow to run cps without the performance penalty of mixing
 * qml closures and ml closures *)
type t = Obj.t
let is_closure _f =
  let tag = Obj.tag (Obj.repr _f) in tag = Obj.closure_tag || tag = Obj.infix_tag

let check _ = assert false
let assert_ _ = assert false
let show _ = assert false
let apply _f _ = assert false
let apply1 f x =
  assert( if not(is_closure f) then (
    Printf.printf "RT:qmlClosureRuntime:FakeApply1: expected a closure but TAG=%d" (Obj.tag (Obj.repr f)); false
  ) else true);
  (Obj.obj f : _ -> _) x
let apply2 f x y =
  assert( if not(is_closure f) then (
    Printf.printf "RT:qmlClosureRuntime:FakeApply2: expected a closure but TAG=%d" (Obj.tag (Obj.repr f));false
  ) else true);
  (Obj.obj f : _ -> _ -> _) x y
let create _ _ _ = assert false
let create_no_ident _ _ = assert false
let create_no_ident1 = Obj.repr
let create_no_ident2 = Obj.repr
let create_no_function _ _ = assert false
let define_function _ _ = assert false
let is_empty _ = assert false
let get_identifier _ = assert false
let applied _ = assert false
let unapplied _ = assert false
let import _ _ = assert false
let export _ = assert false
let get_args _ = assert (Printf.printf "RT:qmlClosureRuntime:Fake.get_args: use opa closures !!");false
let get_tyargs _ = assert (Printf.printf "RT:qmlClosureRuntime:Fake.get_tyargs: use opa closures !!");false

#<Else>

(*-----------------------*)
(*----- typedefs --------*)
(*-----------------------*)
type t_extra = {
  ty_args : AnyArray.t; (* a type for each arg *)
}
type t = { (* the type of closure must be monomorphic
            * or else generalization problem will be really troublesome *)
  arity : int;
  mutable identifier : Obj.t option; (* the name of the function (a backend record) if any *)
  args: AnyArray.t; (* the previously applied arguments *)
  mutable func: Obj.t; (* the 'code pointer', it is mutable because we sometimes create the closure
                        * without the code pointer, and then we fill it
                        * this field will be set either once,
                        * or one time with a dummy value and the second time with the real value *)
  extra : t_extra  (* all basic closure extension are regroupped here to keep a small standard record for t *)
}

(*--------------------------------------*)
(*------- printing/runtime check -------*)
(*--------------------------------------*)

(* check that a given object has a runtime representation compatible with a closure *)
let check_closure_arity t =
  Obj.is_int (Obj.field t 0) && (Obj.obj t : int) > 0
let check_closure_identifier t =
  let option = (Obj.field t 1) in
  if Obj.is_block option then (
    Obj.size option = 1 &&
      let obj_t = (Obj.field option 0) in
      (* QMLFLAT SPECIFIC BEGIN *)
      Obj.is_block obj_t &&
      Obj.tag obj_t = 0 &&
      (let ss = Obj.size obj_t in ss = 3 || ss = 4) &&
      Obj.tag (Obj.field obj_t 0) = 245 && (* vtable -> array *)
      (let ss = Obj.size (Obj.field obj_t 0) in ss = 1 || ss = 2) && (* vtable -> array of size 1 or 2 *)
      DebugPrint.option (Obj.field obj_t 1) &&
      Obj.tag (Obj.field obj_t 2) = Obj.string_tag
      (* QMLFLAT SPECIFIC END *)
  ) else
    option = Obj.repr None
let check_closure_args t =
  Obj.is_block (Obj.field t 2) && (Obj.tag (Obj.field t 2) = 0 || Obj.tag (Obj.field t 2) = 245)
let check_closure_func t =
  let tag = Obj.tag (Obj.field t 3) in
  tag = Obj.closure_tag || tag = Obj.infix_tag (* infix happens in recursive functions *)

let check : 'a -> bool =
  fun t' ->
    let t = Obj.repr t' in
    Obj.is_block t &&
    Obj.tag t = 0 &&
    Obj.size t = 5 &&
    check_closure_arity t &&
    check_closure_identifier t &&
    check_closure_args t &&
    check_closure_func t
let assert_ : 'a -> t = fun t -> assert (check t); (Obj.magic t : t)

let show_gen ?(rec_=false) closure =
  if check closure then
    (* checking that we really have a closure before pattern matching it *)
    let { identifier=identifier
        ; arity=arity
        ; args=args
        ; func=_func
        ; extra=extra } = assert_ closure in
    let string =
      Printf.sprintf "{identifier=%s; arity=%d; args=#%d[|%s|]; t_extra=#%d[|%s|]; func=_}"
        (match identifier with
         | None -> "None"
         | Some id -> DebugPrint.print id)
        arity
        (Array.length args)
        (if rec_ then
           (Base.String.concat_map ";" DebugPrint.print (Array.to_list args))
         else
           "...")
        (Array.length extra.ty_args)
        (if rec_ then
           (Base.String.concat_map ";" DebugPrint.print (Array.to_list extra.ty_args))
         else
           "...") in
    Some string
  else
    None
let () = DebugPrint.register {DebugPrint.f = (fun closure -> show_gen ~rec_:true closure)}

let show closure =
  match show_gen ~rec_:true closure with
  | None -> Base.failwithf "Expected a closure but got %s" (DebugPrint.print closure)
  | Some s -> s

let show_ml_closure_field f =
  let f' = Obj.repr f in
  assert (Obj.tag f' = Obj.closure_tag);
  for i = 0 to Obj.size f' - 1 do
    Printf.printf "field %d: %s
" i (Base.Obj.dump (Obj.field f' i));
  done

(*------------------------------------*)
(*-------- allocation of closures ----*)
(*------------------------------------*)

(* this function will be used to fill the field 'func' for closures defined in two steps *)
let dummy_function _ = assert false
let empty_t_extra = { ty_args= AnyArray.empty }

let create_raw f n identifier =
  let closure =
    { func = Obj.repr f;
      arity = n;
      args = AnyArray.create 0;
      identifier = identifier;
      extra = empty_t_extra
    } in
  #<If> assert_ closure (* this checks that the checking
                         * function is up to date
                         * (and that the identifier is valid) *)
  #<Else> closure
  #<End>

let create f n identifier = create_raw f n (Some (Obj.repr identifier))


(* convert a function that expects a single argument that is an anyarray
 * into a function that expects the arguments one by one *)
let anyarray_fun_to_fun arity fun_ =
  let remaining = arity in
  if remaining = 0 then
    fun () -> (Obj.magic fun_ : _ -> _) [||]
  else
    let rec aux left acc =
      if left = 1 then
        acc
      else
        let acc = fun prev arg -> acc (AnyList.push prev arg) in
        aux (left - 1) (Obj.magic acc) in
    let acc =
      fun prev arg ->
        let anylist = AnyList.push prev arg in
        (Obj.magic fun_ : _ -> _) (AnyList.to_anyarray anylist) in
    let acc = aux remaining acc in
    acc AnyList.empty

let create_anyarray f n identifier =
  (* closures created by that functions are assumed not to take environments *)
  let f = anyarray_fun_to_fun n f in
  create_raw f n (Some (Obj.repr identifier))

let create_no_ident f n = create_raw f n None
let create_no_ident1 f = create_no_ident f 1
let create_no_ident2 f = create_no_ident f 2

let create_no_function n identifier = create dummy_function n identifier
let define_function closure fun_ =
  assert (closure.func == Obj.repr dummy_function); (* making sure that we can't update the closure twice *)
  closure.func <- Obj.repr fun_


(*-----------------------------*)
(*-- application of closures --*)
(*-----------------------------*)
let array1 v = (Obj.magic {tuple1 = v} : Obj.t array)
let array2 v1 v2 = (Obj.magic (v1, v2) : Obj.t array)

let check_env_apply clos args =
  assert (check clos);
  assert (Array.length clos.args = 0);
  assert (clos.arity >= Array.length args)

let env_apply clos args =
  #<If>
    check_env_apply clos args
  #<End>;
  {clos with args = args}

let env_apply_with_ty clos args ty_args =
  #<If>
    check_env_apply clos args
  #<End>;
  {clos with args = args; extra = {(*clos.extra with *)ty_args = ty_args} }


let env_apply1 clos arg1 = env_apply clos (array1 arg1)
let env_apply2 clos arg1 arg2 = env_apply clos (array2 arg1 arg2)

let env_apply1_with_ty clos arg1 ty_arg1= env_apply_with_ty clos (array1 arg1) (array1 ty_arg1)
let env_apply2_with_ty clos arg1 arg2 ty_arg1 ty_arg2 = env_apply_with_ty clos (array2 arg1 arg2) (array2 ty_arg1 ty_arg2)

let args_apply clos args =
  #<If>
    assert (check clos);
    if not (clos.arity = Array.length clos.args + Array.length args) then (
      Printf.printf "CLOSURE: %s\nARGS: %s\n%!" (DebugPrint.print clos) (DebugPrint.print args);
      assert false
    )
  #<End>;
  let f = ref (Obj.obj clos.func) in
  let env = clos.args in
  if Array.length args = 0 then (
    if Array.length env = 0 then
      !f ()
    else (
      let n = Array.length env in
      for k = 0 to n - 2 do
        f := Obj.magic (!f (AnyArray.get env k))
      done;
      (Obj.magic !f : _ -> _) (AnyArray.get env (n-1)) (* tail call *)
    )
  ) else (
    for k = 0 to Array.length env - 1 do
      f := Obj.magic (!f (AnyArray.get env k))
    done;
    let n = Array.length args in
    for k = 0 to n - 2 do
      f := Obj.magic (!f (AnyArray.get args k))
    done;
    (Obj.magic !f : _ -> _) (AnyArray.get args (n-1)) (* tail call *)
  )

(* specialized, more efficient version of the function above
 * these functions are generated by qmlClosure, but some of these are hard written here
 * so that we can call them (for efficiency) *)
let args_apply1 closure a0 =
  #<If>
    assert (check closure);
    if not (closure.arity = Array.length closure.args + 1) then (
      Printf.printf "CLOSURE: %s\nARG: %s\n%!" (DebugPrint.print closure) (DebugPrint.print a0);
      assert false
    )
  #<End>;
  match closure.args with
  | [||] -> (Obj.magic closure.func : _ -> _) a0
  | [|e0|] -> (Obj.magic closure.func : _ -> _ -> _) e0 a0
  | [|e0; e1|] -> (Obj.magic closure.func : _ -> _ -> _ -> _) e0 e1 a0
  | [|e0; e1; e2|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _) e0 e1 e2 a0
  | [|e0; e1; e2; e3|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 a0
  | [|e0; e1; e2; e3; e4|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 a0
  | [|e0; e1; e2; e3; e4; e5|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 e5 a0
  | [|e0; e1; e2; e3; e4; e5; e6|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 e5 e6 a0
  | [|e0; e1; e2; e3; e4; e5; e6; e7|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 e5 e6 e7 a0
  | [|e0; e1; e2; e3; e4; e5; e6; e7; e8|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 e5 e6 e7 e8 a0
  | [|e0; e1; e2; e3; e4; e5; e6; e7; e8; e9|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 a0
  | _env ->
      #<If>Printf.printf "QmlClosureRuntime: falling through apply1: env:%d arity:%d\n%!" (Array.length _env) closure.arity#<End>;
      args_apply closure (Obj.magic {tuple1=a0})

let args_apply2 closure a0 a1 =
  #<If>
    assert (check closure);
    assert (closure.arity = Array.length closure.args + 2);
  #<End>;
  match closure.args with
  | [||] -> (Obj.magic closure.func : _ -> _ -> _) a0 a1
  | [|e0|] -> (Obj.magic closure.func : _ -> _ -> _ -> _) e0 a0 a1
  | [|e0; e1|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _) e0 e1 a0 a1
  | [|e0; e1; e2|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 a0 a1
  | [|e0; e1; e2; e3|] -> (Obj.magic closure.func : _ -> _ -> _ -> _ -> _ -> _ -> _) e0 e1 e2 e3 a0 a1
  | _env ->
      #<If>Printf.printf "QmlClosureRuntime: falling through apply2: env:%d arity:%d\n%!" (Array.length _env) closure.arity#<End>;
      args_apply closure (Obj.magic (a0,a1))

(*---------------------------*)
(*-- api for serialization --*)
(*---------------------------*)

(*
 * A closure is empty when it has not been applied any arguments
 *(which includes environment 'arguments')
 * Same remark as for get_identifier: you need to provide a closure,
 * but it can't be enforced in the type
 *)
let is_empty obj =
  let closure = #<If>assert_ obj#<Else>(Obj.magic obj : t)#<End> in
  Array.length closure.args = 0

(*
 * [get_identifier closure] really takes a closure in spite of its type
 * but at the point where you insert the bypass, the function is not yet
 * a closure and so the bypass would be unusable
 * Saying (_ -> _) -> _ option wouldn't work since closures would end up projecting the function
 *)
let get_identifier obj =
  let closure = #<If>assert_ obj#<Else>(Obj.magic obj : t)#<End> in
  Obj.magic closure.identifier

let set_identifier closure value =
  closure.identifier <- Some (Obj.repr value)

let get_args t = t.args
let get_tyargs t = t.extra.ty_args

(*--------------------------*)
(*------- bsl proj ---------*)
(*--------------------------*)

let import : 'a -> int -> t = create_no_ident
let export : t -> 'a = fun closure ->
  let env = closure.args in
  let env_size = Array.length env in
  if closure.arity = env_size then
    (* exporting a [-> 'a] *)
    Obj.magic (fun () -> args_apply closure [||])
  else (
    let f = ref (Obj.magic closure.func) in
    for i = 0 to env_size - 1 do
      f := (Obj.magic !f : _ -> _) (AnyArray.get env i)
    done;
    !f
  )

#<End>
