open SurfaceAst

let identity = Base.identity

module Fixpoint =
struct
  (* used for removing nodes *)
  let rec repeat f e =
    let e' = f e in
    if e == e' then e' else repeat f e'
  (* could avoid some comparisons by saying
   * let rec repeats fs e_o =
   *   let rec aux ... =
   *     match ... with
   *     | [] -> it e == e_0 then e else repeats fs e
   *)
  let repeats fs e =
    let rec aux e l did_something =
      match l with
        | [] -> if did_something then init e else e
        | h :: t ->
          let e' = repeat h e in
          aux e' t (did_something || e != e')
    and init e = aux e fs false in
    init e

  (* used for removing nodes and getting the context *)
  let rec repeat' f (e,recombiner) =
    let e', recombiner' = f e in
    let recombiner'' = if recombiner' == identity then recombiner else fun e -> recombiner (recombiner' e) in
    if e == e' then
      e, recombiner''
    else
      repeat' f (e',recombiner'')

  let repeats' fs e =
    let rec aux e l did_something =
      match l with
        | [] -> if did_something then init e else e
        | h :: t ->
          let e' = repeat' h e in
          aux e' t (did_something || fst e != fst e')
    and init e = aux e fs false in
    init e
end


module Remove =
struct
  type ('a,'b) through = ('a, 'b) expr -> ('a, 'b) expr
  let fail label = failwith (Printf.sprintf "SurfaceAstDecons.Remove.fail: %s" (FilePos.to_string label.QmlLoc.pos))

  module Basic =
  struct
    let access_directive = function
      | (Directive (#access_directive,[e],_),_) -> e
      | (Directive (#access_directive,_,_),_) -> assert false
      | e -> e
    let access_not_public = function
      | (Directive (`public,_,_),_) as e -> e
      | (Directive (#access_directive,[e],_),_) -> e
      | (Directive (#access_directive,_,_),_) -> assert false
      | e -> e
    let async = function
      | (Directive (`async, [e], _),_) -> e
      | (Directive (`async, _, _),_) -> assert false
      | e -> e
    let binding_directive = function
      | (Directive (#binding_directive, [e], _),_) -> e
      | (Directive (#binding_directive, _, _),_) -> assert false
      | e -> e
    let coerce = function
      | (Directive (`coerce,[e],_),_) -> e
      | (Directive (`coerce,_,_),_) -> assert false
      | e -> e
    let deprecated = function
      | Directive (`deprecated, [ _ ; e ], _), _ -> e
      | Directive (`deprecated, _, _), _ -> assert false
      | e -> e
    let directive = function
      | (Directive (_,[e],_),_) -> e
      | e -> e
    let doctype = function
      | (Directive (`doctype _,[e],_),_) -> e
      | (Directive (`doctype _,_,_),_) -> assert false
      | e -> e
    let expand = function
      | (Directive (`expand _, [e], _), _) -> e
      | (Directive (`expand _, _, _), _) -> assert false
      | e -> e
    let magic_directive = function
      | Directive (`specialize _, e::_, _), _ -> e
      | Directive (#magic_directive, [e], _), _ -> e
      | Directive (#magic_directive, _, _), _ -> assert false
      | e -> e
    let lambda = function
      | (Lambda (_,e),_) -> e
      | e -> e
    let letin = function
      | (LetIn (_,_,e),_) -> e
      | e -> e
    let opacapi = function
      | (Directive (`opacapi, [e], _), _) -> e
      | (Directive (`opacapi, _, _),_) -> assert false
      | e -> e
    let opavalue_directive = function
      | (Directive (#opavalue_directive, [e], _), _) -> e
      | (Directive (#opavalue_directive, _, _),_) -> assert false
      | e -> e
    let open_ = function
      | (Directive (`open_, [_;e], _),_) -> e
      | (Directive (`open_, _, _),_) -> assert false
      | e -> e
    let private_ = function
      | (Directive (`private_, [e], _),_) -> e
      | (Directive (`private_, _, _),_) -> assert false
      | e -> e
    let slicer_directive = function
      | (Directive (#distribution_directive,[e],_),_) -> e
      | (Directive (#distribution_directive,_,_),_) -> assert false
      | e -> e
    let side_annotation = function
      | (Directive (`side_annotation _,[e],_),_) -> e
      | (Directive (`side_annotation _,_,_),_) -> assert false
      | e -> e
    let visibility_annotation = function
      | (Directive (`visibility_annotation _,[e],_),_) -> e
      | (Directive (`visibility_annotation _,_,_),_) -> assert false
      | e -> e
    let workable = function
      | (Directive (`workable, [e], _),_) -> e
      | (Directive (`workable, _, _),_) -> assert false
      | e -> e


  end
  let remove ~through e = Fixpoint.repeats through e
  let coerce e = Fixpoint.repeat Basic.coerce e
  (* with conjunctive types, this could be well typed
   * but then you would need `coerce None; `wrap None *)
  (* let rec translate = function
    | `coerce -> [Basic.coerce]
  and translates v = List.concat_map translate v
  let general ~through e =
    let through = translates through in
    Fixpoint.repeats ~through e *)
end

module Look =
struct
  type ('a,'b) through = ('a,'b) Remove.through
  let default_removals = []
  let apply ?(through=default_removals) e =
    match Fixpoint.repeats through e with
      | (Apply _,_) -> true
      | _ -> false
  let lambda ?(through=default_removals) e =
    match Fixpoint.repeats through e with
    | (Lambda _,_) -> true
    | _ -> false
  let module_ ?(through=default_removals) e =
    match Fixpoint.repeats through e with
    | (Directive (`module_,_,_),_) -> true
    | _ -> false
  let private_ ?(through=default_removals) e =
    match Fixpoint.repeats through e with
    | (Directive (`private_,_,_),_) -> true
    | _ -> false
  let record ?(through=default_removals) e =
    match Fixpoint.repeats through e with
    | (Record _,_) -> true
    | _ -> false
  (* assume that the module has local directive, not that it is a local module *)
  let module_local ?(through=default_removals) e =
    match Fixpoint.repeats through e with
      | (Directive (`module_,[(Record ((_,(Directive (`local _,_,_),_))::_),_)],_),_) -> true
      | _ -> false
  let at ?(through=default_removals) ~at e =
    let e = Fixpoint.repeats through e in
    let e' = Fixpoint.repeats at e in
    e != e'
end

module Context =
struct
  type ('a,'b,'c) through_with_context = ('a,'b) expr -> ('a, 'b) expr * (('a, 'c) expr -> ('a, 'c) expr)
  (* put them in alphabetical order *)
  module Basic =
  struct
    let coerce = function
      | (Directive (`coerce,[e],b),c) -> e, (fun e -> (Directive (`coerce,[e],b),c))
      | (Directive (`coerce,_,_),_) -> assert false
      | e -> e, identity
    let directive = function
      | (Directive (a,[e],c),d) -> e, (fun e -> (Directive (a,[e],c),d))
      | e -> e, identity
    let lambda = function
      | (Lambda (a,e),b) -> e, (fun e -> (Lambda (a,e),b))
      | e -> e, identity
    let letin = function
      | (LetIn (rec_,a,e),b) -> e, (fun e -> (LetIn (rec_,a,e),b))
      | e -> e, identity
    let opavalue_directive = function
      | (Directive (#opavalue_directive as v,[e],b),c) ->
          e,  (fun e -> (Directive (v,[e],b),c))
      | (Directive (#opavalue_directive,_,_),_) -> assert false
      | e -> e, identity
    let opacapi = function
      | (Directive (`opacapi,[e],c),d) -> e, (fun e -> (Directive (`opacapi,[e],c),d))
      | (Directive (`opacapi, _, _),_) -> assert false
      | e -> e, identity
    let open_ = function
      | (Directive (`open_,[b;e],c),d) -> e, (fun e -> (Directive (`open_,[b;e],c),d))
      | (Directive (`open_, _, _),_) -> assert false
      | e -> e, identity
    let doctype = function
      | (Directive (`doctype arg,[e],b),c) -> e,  (fun e -> (Directive (`doctype arg,[e],b),c))
      | (Directive (`doctype _,_,_),_) -> assert false
      | e -> e, identity
    let slicer_directive = function
      | (Directive (#distribution_directive as v,[e],b),c) ->
          e,  (fun e -> (Directive (v,[e],b),c))
      | (Directive (#distribution_directive,_,_),_) -> assert false
      | e -> e, identity
    let binding_directive = function
      | (Directive (#binding_directive as v,[e],b),c) ->
          e,  (fun e -> (Directive (v,[e],b),c))
      | (Directive (#binding_directive,_,_),_) -> assert false
      | e -> e, identity
    let side_annotation = function
      | (Directive (`side_annotation _ as v,[e],b),c) ->
          e,  (fun e -> (Directive (v,[e],b),c))
      | (Directive (`side_annotation _,_,_),_) -> assert false
      | e -> e, identity
    let visibility_annotation = function
      | (Directive (`visibility_annotation _ as v,[e],b),c) ->
          e,  (fun e -> (Directive (v,[e],b),c))
      | (Directive (`visibility_annotation _,_,_),_) -> assert false
      | e -> e, identity
  end
  let remove ~through e = Fixpoint.repeats' (through : (_,_,_) through_with_context list) (e,identity)
  let filter
      ~(keep:('a, 'b, 'c) through_with_context list)
      ~(throw:('a, 'b) Remove.through list) e
      : ('a, 'b) SurfaceAst.expr * (('a, 'c) SurfaceAst.expr -> ('a, 'c) SurfaceAst.expr) =
    let rec aux acc e =
      let e' = Fixpoint.repeats throw e in
      let e'',acc = Fixpoint.repeats' keep (e',acc) in
      if e == e'' then
        e, acc
      else
        aux acc e'' in
    aux identity e
  let filter2 ~keep1 ~keep2 ~throw e =
    let rec aux acc1 acc2 e =
      let e' = Fixpoint.repeats throw e in
      let e'',acc1 = Fixpoint.repeats' keep1 (e',acc1) in
      let e''',acc2 = Fixpoint.repeats' keep2 (e'',acc2) in
      if e == e''' then
        e, acc1, acc2
      else
        aux acc1 acc2 e''' in
    aux identity identity e
  let uncoerce e = Fixpoint.repeat' Basic.coerce (e,identity)
  let unletin e = Fixpoint.repeat' Basic.letin (e,identity)
end

module FoldThrough =
struct
  (* faire une contrepartie dans surfaceAstCons *)
  let default_removals = Look.default_removals
  let dot ?(through=default_removals) e =
    let rec aux acc e =
      match Fixpoint.repeats through e with
        | (Dot (e,s),lab) -> aux ((s,lab)::acc) e
        | e -> e, acc in
    aux [] e
  let arity ?(through=default_removals) e =
    match Fixpoint.repeats through e with
      | (Lambda (r,_),_) -> Some (List.length r)
      | _ -> None
  let fields ?(through=default_removals) e =
    match Fixpoint.repeats through e with
      | (Record r,_)
      | (Directive (`module_, [(Record r, _)], _),_) -> Some r
      | _ -> None
end

module FoldContext =
struct
  let default_removals = Look.default_removals
  let letin ?(through=default_removals) e =
    let rec aux acc e =
      match Fixpoint.repeats' through e with
        (* BEWARE: dropping the rec_ flag *)
        | ((LetIn (_,bindings,e),_),recombiner) -> aux (List.rev_append bindings acc) (e,recombiner)
        | (e,recombiner) -> e, acc, recombiner in
    aux [] (e,identity)
end
