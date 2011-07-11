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

   NOT UP TO DATE JUNK :

   Strict case and non strict case are of different nature (the size is not known in the latter case).
   We try to elminate unstrict case when possible

   Remaining unstrict case
   We do the strict case analysis and non strict separately




   Or of And normalisation :

   First we regroup all case on the strict together,
   for this we need to be able to make them go before non strict case, this is done by duplicating them.

   Non strict case are gives a set of strictified version (merge with strict cases) and a the original one

   Strict record case (including previously generated one) are regroup (order of group has no importance).

   All non strict record case in kept in the original order

   e.g. cases are re-organised as follow :


   match ...
   { toto=1 }         -> case1
   { toto=2 bibi=3 }  -> case2
   { bibi=4 ... }     -> case3
   { toto=5 ... }     -> case4

   =>
   failure = ...
   match ...
   { toto = x } -> match x | 1 -> case1'
                           | 5 -> case4'
                   end
   { bibi = x bibi = y } -> match x | 2 -> match y | 3 -> case2'
                                           end
                                    | 4 -> case3'
                            end
   { bibi=x ... } -> match x | 4 -> case3' end
   { toto=x ... } -> match x | 5 -> case4' end

   This help to regroup all complete case, case code can be shared or not depending of the low level implementation

   Algorithm
   regroup all strict possible cases (fields,bool) keeping intra-order information

   strictify non strict case on the basis of existing strict case




   Or of const are given a default case with Failure code
*)

(* depends *)
let (|>) = InfixOperator.(|>)
module Format = Base.Format
module List = Base.List

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst

open Loop.Deprecated

let debug_flag = false

let debug fmt =
  Format.printf "%!";
  if debug_flag then Format.printf fmt
  else Format.ifprintf Format.std_formatter fmt

(* checking that a list is ordered and with no duplicate *)
let rec ordered l =
  match l with
  | x :: y :: rl ->
      if x<y then ordered rl
      else if y>x then failwith "not sorted"
      else failwith "duplicate"
  | _ -> true

(* same on assoc list *)
let rec ordered2 l =
  match l with
  | (x,_) :: (y,_) :: rl ->
      if x<y then ordered2 rl
      else if y>x then failwith "not sorted"
      else failwith "duplicate"
  | _ -> true

(* a check function returning the list for debug purpose *)
let check l =
  if not( ordered2 (List.rev l)) then (
    Printf.printf "%s\n%!" (String.concat "," (List.map fst l));
    assert false
  );
  l

(* collect all field of strict not appearing in nonstrict (case A) and
   verify that all nonstrict field are in strict (case B) *)
let rec fields_completion (gen:string->'a) (strict:string list) (nonstrict:(string*'a) list) res =
  assert(ordered strict);
  assert(ordered2 nonstrict);
  (* Printf.printf "\n[";
     l_iter(strict)(Printf.printf "%s ");
     Printf.printf "\n[";
     l_iter(nonstrict)(fun (f,_)->Printf.printf "%s "f);
     Printf.printf "]\n";
  *)
  let rec aux strict nonstrict res =
    match strict,nonstrict with
    (* A *)
    | f0::strict, (f1,_)::_         when f0<f1 -> aux strict nonstrict (check((f0,gen f0)::res))
    | f0::strict, []                           -> aux strict nonstrict (check((f0,gen f0)::res))
    | f0::strict, (f1,o)::nonstrict when f0=f1 -> aux strict nonstrict (check((f1,o       )::res))

    (* terminaison *)
    | [], []  -> Some (List.rev res)

    (* B *)
    | f0::_     , (f1,_)::_         when f0>f1 -> None
    | []        , _::_                         -> None

    (* ocaml [stupid] complain *)
    | _::_          , _::_                         -> assert false
  in
  let res = aux strict nonstrict res in
  assert(Option.default true (Option.map ordered2 res));
  res



module Onion ( Lang : QmlPatternAnalysisSig.ONIONLANG) =
struct

module L = Lang
type field = string


type bind = { ident : L.ident list ; coerce_ty : L.ty list} (* standard binding, support name and coercion *)

(*
  type switch = Const | NoField | Immediate IsRecord_0  IsPrt
*)

(** immediate, checkable property *)
type pat =
  | Any                         (* the easy property, equivalent exist for fields, see And *)
  | Cst of L.const                (* constant property *)
      (*  | Rec of ('bind) layer_record (* set of field property *) *)
      (*  | Backend of 'backend_property *)



(** composition of checkable properties *)
type onion =
    (** basic algebra *)
  | Or   of onion_or               (* to match sum    *)
  | And  of onion_and              (* to match record *)
  | Term of onion_term             (* terminal *)


(** pattern disjunction *)
and onion_or =
    {
      cases   : onion list  ;   (* list of patterns *)
      default : onion_term option;   (* the pattern is useless *)
      ty      : L.ty option  ; (* sadly we have to use type to limits presence of '...' and determines completness *)
    }

(** pattern conjunction, each conjunction operand is actually associated to a field, but can be extended to any conjuction kind *)
and onion_and =
    {
      fields : (field*onion) list ; (* check are associated to field *)
      strict : bool               ; (* pattern contains ... *)
      term   : onion_term         ; (* binded recursion, the pattern is useless !! *)
    }

(** terminal pattern
    binding occurs and submatch occurs here
*)
and onion_term =
    {
      bind    : bind; (* bind names and coerce *)
      pat     : pat ; (* the terminal patern *)
      content : recur; (* recursion, can content the final expr or an explicit submatch *)
    }

(** expression and submatch *)
and recur =
    Expr of L.expr
  | Failure of string
      (* the latter two should be replaced by Branch x where
         x is the number of the branch of the whole pattern matching (i.e. code are numbered)
      *)
  | End (* to fill holes for parten without expr or submatch *)

  (* to nest pattern explicit tree form pattern matching,
     during optimisation, normalisation ...

     ident is the input value of pattern matchin
     or a sentinel value to indicate that joint pattern matching must be continuated
  *)
  | Recur of L.ident * L.ty option * onion

  (*  | BranchSet of IntSet.t
      | Branch of int (* int > 0 *)
      | JointSet of integer list *)

(**
   Add a binding to an onion
*)
let add_bind ident bind =
  { bind with
      ident = ident :: bind.ident
  }

let add_bind_term ident term =
  { term with
      bind = add_bind ident term.bind ;
  }

let add_bind_onion ident = function
  | Or _ ->
      (*
        TODO
        Currently, this is not implemented, even in qml
      *)
      assert false

  | And onion_and ->
      let onion_and = {
        onion_and with
          term = add_bind_term ident onion_and.term ;
      } in
      And onion_and

  | Term term -> Term (add_bind_term ident term)

(** getting and_ in And and_ *)
let get_and =
  function And and_ -> and_
  | Or _
  | Term _ -> raise (Invalid_argument "get_and")

(** getting term in Term term *)
let get_term =
  function Term term-> term
  | And _ -> assert false
  | Or _ -> assert false

(** getting the term in Term term, And { term=term } *)
let get_onion_term =
   function Term term | And { term = term } -> term
  | Or _ -> assert false

let get_fields =
   function And { fields = fields} -> fields
  | _ -> []


(** some handy constructors *)
let bind ?(id=[]) ?(ty=[]) () = {ident=id;coerce_ty=ty}
let bind_var i = bind ~id:[i] ()
let no_bind = bind ()

let term_r ?(bind=no_bind) ?(pat=Any) ?(e=End) ()= {bind=bind;pat=pat; content=e}
let term ?(bind=no_bind) ?(pat=Any) ?(e=End) () = Term (term_r ~bind ~pat ~e ())
let term_var i= term ~bind:(bind_var i) ()
let no_term_r = term_r ()
let no_term = term ()

(** A S2 module for basic needs *)
module S2 = struct
  type 'p t = onion constraint 'p = (_ * _ * _)

  let stable2 a a' b b' = a==a' && b==b'

  let snd_fold_map tra acc ((a,b) as c) =
    let acc,b' =tra acc b in
    acc,if b'==b then c else (a,b')

  let fold_map_recur tra acc r =
    match r with
    | Recur (id,ty,o)  ->
        let acc, o' = tra acc o in
        if o'==o then acc,r else acc,Recur(id,ty,o')
    | _ -> acc,r

  let foldmap tra acc o =
    match o with
    | Or { cases=cases ; default=default ; ty = ty} ->
        let acc, cases'   = List.fold_left_map_stable tra acc cases  in
        let acc, default' = Option.foldmap_stable (fun acc d->
          let acc,c'=fold_map_recur tra acc d.content in
          if c'==d.content then acc,d
          else acc, {d with content=c'}
        ) acc default
        in
        acc, if stable2 cases cases' default default' then o else Or {cases=cases' ; default=default' ; ty = ty }

    | And ({ fields= fields ; term = { content=content } as term } as and_) ->
        let tra_field = snd_fold_map tra in
        let acc, fields'  = List.fold_left_map_stable tra_field acc fields  in
        let acc, content' = fold_map_recur         tra       acc content in
        acc, if stable2 fields fields' content content' then o else And { and_ with fields= fields' ; term =  { term with content=content' } }

    | Term ({content=content} as term) ->
        let acc, content' = fold_map_recur tra acc content in
        acc, if content'==content then o else Term {term with content=content'}



  let iter x = Traverse.Unoptimized.iter foldmap x
  let map x = Traverse.Unoptimized.map foldmap x
  let fold x = Traverse.Unoptimized.fold foldmap x

end


module Trav = Traverse.Make2( S2 )


(** printers for external types *)
let print_id = Lang.print_id
let print_ty = Lang.print_ty
let print_expr = Lang.print_expr
let print_const = Lang.print_const


(** printers for internal types *)
let rec print_recur fmt r =
  match r with
  | End -> ()
      (*      | JointSet s->  Format.fprintf fmt "@[{%a}@]" (Format.pp_list " , " Format.pp_print_int) s *)
  | Failure(str) -> Format.fprintf fmt " Failure : %s" str
  | Recur (f,_ty,r) ->   Format.fprintf fmt  "@\n@[@<2>match @[%a@] @\n @[%a@] @\nend@]@\n" print_id f print r
  | Expr e -> Format.fprintf fmt "@[@ %a@]" print_expr e

and print_pat ~show_pat  fmt pat =
  match pat with
  | Any -> Format.pp_print_string fmt (if show_pat then "_" else "")
  | Cst c -> print_const fmt c

and print_term ~show_pat fmt t =
  let { bind = { ident = il; coerce_ty = tyl }; pat = pat ; content = r } = t
  in
  Format.fprintf fmt  "%a%s%a%s%a%s%a"
    (print_pat ~show_pat) pat
    (if il = [] || pat<>Any then "" else " as ") (Format.pp_list "," print_id) il
    (if tyl= [] then "" else " : " ) (Format.pp_list "," print_ty) tyl
    (if r=End   then "" else "->")  print_recur r

and print_field fmt (field , o ) = Format.fprintf fmt  "@[@ %s = %a @]" field print o
and print fmt o =
  match o with
    | Or {cases=l1 ; default=opt} ->
        Format.fprintf fmt  "@[@ [ %a %a %a ] @]"
          (Format.pp_list "@\n" print) l1
          (fun f o-> if o then Format.fprintf f "@\n #") (opt<>None (*&& l1<>[]*))
          (fun f o -> Option.iter (print_term ~show_pat:true f) o) opt

    | And {fields=fs ; strict=b; term=term}  ->
        Format.fprintf fmt  "AND {@[%a %s@]} %a" (Format.pp_list " ;@ " print_field) fs (if b then "" else  "; ... ") (print_term  ~show_pat:false) term

    | Term t -> Format.fprintf fmt  "@[T %a@]" (print_term ~show_pat:true)   t



(* short and precise pattern are smaller,
   slow because of fields comparison
 *)
let compare p1 p2 =
  match p1,p2 with
  | Or _ , _ -> assert false
  |  _ , Or _ -> assert false

  | Term {pat = Cst x}, Term {pat = Cst y} -> compare x y
  | _, Term {pat = Cst _} -> -1

  | And { fields = fields1; strict=strict1 }, And { fields = fields2; strict=strict2 }
      ->
      if strict1 = strict2 then (
        let len1 = List.length fields1 in
        let len2 = List.length fields2 in
        if len1 = len2 then
          compare (List.map fst fields1) (List.map fst fields2)
        else
          compare len1 len2
      ) else
            if strict1 then -1 else 1

  | _,And _ -> -1

  | Term { pat = Any} , Term { pat = Any} -> 0
  | _ , Term { pat = Any} -> -1

let update_End new_  =
  (* no information loss *)
  let aux tra o =
  match o with
  | And( { term = { content = End} as recur } as and_ ) ->  And { and_ with term = { recur with content = new_ }}
  | Term( { content=End } as term) -> Term {term with content= new_ }
  | And _ -> assert false
  | Term _ -> assert false
  | _ -> tra o
  in Trav.traverse_map aux

(* never traverse term *)
let update_First_Term new_  =
  (* accept information loss *)
  let aux tra o =
  match o with
  | And( { term = { content =  _ } as recur } as and_ ) ->  And { and_ with term = { recur with content = new_ }}
  | Term( { content = _ } as term) -> Term {term with content= new_ }
  | _ -> tra o
  in Trav.traverse_map aux

let add_ty_in_term ty o=
  let add_ty_bind ty bind = {bind with coerce_ty = ty::bind.coerce_ty} in
  let add_ty_term ty term = {term with bind = add_ty_bind ty term.bind } in
  let add_ty_and_ ty and_ = {and_ with term = add_ty_term ty and_.term } in
  match o with
  | And and_    -> And (add_ty_and_ ty and_)
  | Term term  -> Term (add_ty_term ty term)
  | _ -> assert false

module Normalize = struct

  type rpath = field list (* reversed path *)

  type public_exception =
    | Any_before_last_pattern     (* any pattern not in last position, merge with hiding *) (* not used *)
    | Missing_const_case of rpath * L.const list (* incomplete disjonction of constant *)
    | Missing_record_class of rpath * (field list) list
    | Pattern_hidding of recur list
    | Pattern_incompletness of onion list (* only possible with types or jointures ?? *)  (* not used *)


  type private_exception =
    | Empty_pattern
    | Non_homogenious

  type exceptions = Public of public_exception | Private of private_exception

  exception Exc of exceptions list
  (** exceptions :

      public exception are collected, there are actually displayed when flush_exceptions is called,
      the purpose is emission of warning messages

      private exception means internal error, so you should only call them when you feel like something more specific then assert false
      currently they are just raise as other exceptions
  *)
  let ref_public_exception = ref []
  let raise_public  e = ref_public_exception := (Public e)::(!ref_public_exception)
  let has_public_exceptions () = !ref_public_exception<>[]
  let clean_public_exceptions () = ref_public_exception:=[]

  let nb_failure = ref 0
  let nb_total = ref 0

  let raise_private e = raise (Exc[Private e])

  let print_record_class fmt l =  Format.fprintf fmt "{%a}" (Format.pp_list " ; " Format.pp_print_string) l

  let print_rpath fmt rpath =
    if rpath = []  then
      Format.fprintf fmt "in the whole pattern"
    else
      Format.fprintf fmt "in the path %a" (Format.pp_list "." Format.pp_print_string) (List.rev rpath)

  let print_public_exc fmt = function
    | Any_before_last_pattern   -> Format.fprintf fmt "Any_before_last_pattern"
    | Missing_const_case(rpath,lc)    ->
      Format.fprintf fmt "Incomplete constant pattern matching : %a, %a %s"  print_rpath rpath (Format.pp_list " and " print_const) lc
         (if List.length lc = 1 then "constant case is missing"
         else "constants case are missing")
    | Missing_record_class(rpath,lclass)   ->
      Format.fprintf fmt "Incomplete record pattern matching : %a, %a %s" print_rpath rpath (Format.pp_list " and " print_record_class) lclass
        (if List.length lclass = 1 then "record layout is missing"
         else "records layout are missing")
    | Pattern_hidding _lr        ->
        Format.fprintf fmt "Pattern hidding"  (*"before expression %a"  (Format.pp_list " ;@ " print_recur) lr*)
    | Pattern_incompletness _lo -> Format.fprintf fmt "Pattern incompletness TODO" (*(Format.pp_list " ;@ " print_recur) lr*)

  let print_private_exc fmt = function
   | Empty_pattern   ->  Format.fprintf fmt "Empty pattern"
   | Non_homogenious ->  Format.fprintf fmt "Non homogenious pattern"

  let print_exc fmt e =
    match e with
    | Public  e -> print_public_exc fmt e
    | Private e  -> print_private_exc fmt e

  let flush_exceptions_fmt fmt () =
    Format.pp_list "@\n" print_exc fmt (List.rev !ref_public_exception);
    clean_public_exceptions ()


  let flush_exceptions_stdout () = flush_exceptions_fmt  Format.std_formatter ()


(* usefull to avoid nested Or *)
  let rec flatten_or cases =
    l_map_flat(cases)(function
      | Or { cases = l ; default = None } -> l
      | Or { cases = l ; default = Some d} -> (flatten_or l)@[Term d]
      | x -> [x]
    )


(* this function is really linked to the normalisation !! and should be nearer the normalisation *)
  let get_pattern_for_field ty record_name f (and_pattern:onion) =
    match and_pattern with
    | And ({ fields = fields } as and_) ->
        let remainings_fields = List.remove_assoc f fields in
        let _recur = if remainings_fields=[] then and_.term.content else Recur(record_name,ty, And {and_ with fields = remainings_fields }) in
        (* hack : use the intra-pattern term container to memorise its origin, so that latter the correspondance is not lost *)
        begin match List.assoc_opt f fields with
        | Some p -> Some (update_End _recur p)
        | None ->
            Format.printf "get_pattern_for_field %s %a@\n%!" f print and_pattern;
            Format.printf "BUG@\n%!";
            None
        end
    | _ -> Format.printf "get_pattern_for_field %s %a@\n%!" f print and_pattern; assert false

  let get_patterns_for_field ty record_name f and_patterns = List.filter_map (get_pattern_for_field ty record_name f) (flatten_or(* TODO remove the flatten*) and_patterns)


 (* make the pattern only active on the first layer *)
  let strip_pattern ?(keep_list=[]) o =
    match o with
    | And( { fields= fields } as and_ ) -> And { and_ with fields = l_map(fields)(fun (f,o)-> if List.mem_assoc f keep_list then f,o else f, term()) }
    | Term term -> Term {term with bind=no_bind; pat = Any }
    | _ -> o


  let get_names_and_types_1  o (names,tys) =
    match o with
    | Term { bind = { ident = li ; coerce_ty = lt } }
    | And {term = { bind = { ident = li ; coerce_ty = lt } } }  -> (li@names, lt@tys)
    | _ -> assert false

  module PatternClass = struct
    (**
        pattern classes are a sum-up of the first layer of a pattern
        a pattern is said strict (or belongs to a strict class) if its first layer does not contains forms like '_' or '...'
    *)
    (**
        The type of pattern classes
        '...' in records are just discarded since classes will always be separate into sets of strict and non strict classes
        and non strict record classes always have '...'
    *)
    type t = CRECORD of string list | CST of L.const | ANY

    let print_class fmt t =
      match t with
      | CRECORD l -> Format.fprintf fmt "CRECORD(%s)" (String.concat ";" l)
      | CST c ->     Format.fprintf fmt "CST(%a)" print_const c
      | ANY -> Format.fprintf fmt "ANY"


    (** gives the class of a pattern *)
    let from_pattern p =
      match p with
      | Or _ -> assert false
      | Term { pat = Cst c} -> CST c
      | And and_ ->
          assert(ordered2 and_.fields);
          CRECORD(List.map fst and_.fields)
      | Term { pat = Any } -> ANY


    (** check strictness of a pattern *)
    let is_strict p =
      match p with
      | Or _ -> assert false
      | And { strict = strict } -> strict
      | Term { pat = Any } -> false
      | _ -> true

    (** is_instance_of *)
    let is_instance_of ~candidate ~class_ =
      match candidate,class_ with
      | CRECORD candidate, CRECORD class_ -> candidate=class_
      | _ -> assert false

    (** is_unstrict_instance_of *)
    let is_unstrict_instance_of ~candidate ~class_ =
      match candidate,class_ with
      | CRECORD candidate, CRECORD class_ ->
          l_for_all(candidate)(fun f -> List.mem f class_)
      | _ -> assert false


    (** class comparison *)
    let compare p1 p2 = (* most generic pattern are superior to other *)
      match p1,p2 with
      | ANY, ANY -> 0
      | ANY, _   -> 1

      | CRECORD l0, CRECORD l1 -> Pervasives.compare l0 l1
      | CRECORD _ , CST _     -> 1

      | CST c0    , CST c1    -> Pervasives.compare c0 c1

      | _   -> -1

    (** non typed strictification *)
    let strictify_record_class strict nonstrict = fields_completion (fun _ ->term()) strict nonstrict []

    (** from fields of a non strict instance pattern, and a strict class
        eventually return a strict pattern instanciated from the non strict pattern and belonging to the strict class
        if such a pattern exists
        new fields in the pattern are associated to an Any pattern
    *)
    let strictify_pattern_class strict_class p =
      match strict_class,p with
      | CRECORD cfields, And and_ ->
          Option.map (fun fields -> And { and_ with fields = fields})
            (strictify_record_class cfields and_.fields)

      | CRECORD cfields, Term ({ pat = Any } as term) ->
          Option.map (fun fields -> And {fields = fields ; strict = true ; term = term})
            (strictify_record_class cfields [])

      | CST c,  Term ({ pat = Any } as term) -> Some (Term ({term with pat = Cst c}))

      | CST _, And _ -> None

      | _ , Term { pat = Cst _ } (* Cst cannot be unstrict *)
      | ANY  , _                 (* ANY is not a strict class *)
      | _    , Or _
          -> assert false


    (** typed strictification, superior to the latter but need type information *)
    let strictify_record_ty  ty field_p_l term_ =
      let fields = List.map fst field_p_l in
      let ands_ = l_map(L.strictify_record_ty ty fields)(fun (fields,strict)->
        let fields_o = l_map(fields)(fun field ->
          match List.assoc_opt field field_p_l with
          | None ->
              debug "strictify_record_ty : missing %s replaced by any @\n" field;
              (field,term ())
          | Some o -> (field,o)
        ) in
        And {fields = fields_o ; strict= strict = `closed ; term = term_}, CRECORD fields
      ) in
      ands_

    (** typed strictification, superior to the latter but need type information *)
    let strictify_pattern_ty ty p =
      match p with
      | And and_ when not(and_.strict) -> strictify_record_ty ty and_.fields and_.term
      | Term ({ pat = Any } as term) ->
          debug "Strictify Any  strictify_pattern_ty @\n";
          let r = strictify_record_ty ty [] term in
          if r<>[] then r
          else (* for the any case with const type *)
             [p, from_pattern p]
      | _ ->
          debug "strictify_pattern_ty: %a @\n" print p;
          assert false


    let get_class_fields class_ =
      match class_ with
        | CRECORD cfields -> cfields
        | _ -> assert false


 (* here start the real stuff, move this outside of PatternClass *)
 exception Local_no_recur

   (* TODO Document *)
 let class_merge new_ident _is_joint strict ty  (c,l) =
   let strip_names_1 o =
     match o with
     | Term t -> Term { t with bind = {t.bind with ident =[]}}
     | _ -> o
   in
   let get_names_and_types and_patterns f = (* get all existing coercions and names for the field *)
     l_fold(and_patterns,([],[]))(fun and_pattern acc ->
       let and_ = get_and and_pattern in
       get_names_and_types_1 (List.assoc f and_.fields) acc
     )
    in
   let get_recur (l as initl) =
     let id,l = l_fold(l,(None,[]))(fun pat (id,l) ->
       match pat with
       | Term { content = Recur(id',_,o) }
       | And { term = { content = Recur(id',_,o) } } ->
           assert( Option.default_map true (fun id-> id=id') id );
           ((Some id'),o::l)
       | _ -> debug "ASSERT get_recur: %a\n" (Format.pp_list " |@\n " print) initl; raise  Local_no_recur
     ) in Recur(Option.get id,ty, if List.length l > 1 then Or { cases = List.rev l ; default=None ; ty=ty} else List.hd l)
   in
   let get_field_type fields = match ty with
     | None -> (fun _ -> None)
     | Some ty -> fun field -> Some (L.strict_get_field_type ty fields field)
   in
   debug "class_merge %a\n" print_class c;
   let recurs =  try Some(get_recur l) with Local_no_recur -> None  in
   let ext_ty tys =  match ty with None -> tys | Some ty -> ty::tys in
   match c with
   | CRECORD fields when fields<>[] ->
       let id = new_ident "record_to_recurse" in
       let fields_idents_types = l_map(fields)(fun field ->  field, new_ident field, get_field_type fields field)  in
       let fields = l_map(fields_idents_types)(fun (f,name,ty)->
         let names,tys = get_names_and_types l f in
         let tys = match ty with None -> tys | Some ty -> ty::tys in
         (f,term ~bind:(bind ~id:(name::names) ~ty:tys ()) ())
       ) in
       begin match recurs with
       | Some e when false ->
           let o = And { fields=fields;  strict=strict ; term=term_r ~bind:(bind ~ty:(ext_ty [])()) ~e () } in
           debug "\n\nMerging in %a\n\n" print o;
           Some(id,fields_idents_types),[], o
       | _  ->
           (* the first layer of patterns should be cleaned from bindings since we get them in the main pattern *)
           let l = l_map(l)(strip_names_1) in
           let names,tys = l_fold(l, ([],[]) )(get_names_and_types_1) in
           Some(id,fields_idents_types),l,And { fields=fields;  strict=strict ; term=term_r ~bind:(bind ~id:names ~ty:(ext_ty tys)())  () }
       end

   | _ -> (* includes const, any and {} *)
       begin match recurs with
       | Some e -> (* matching will continue, so we must not discard patterns *)
           if List.length l > 1 then debug "\n\nKeeping usefull patterns because joint %a\n\n" (Format.pp_list "|" print) l;
           let pat = match List.hd l with Term {pat=pat} -> pat | And { fields = [] } -> Any | _ -> assert false in
           let id,tys = l_fold(l,([],[]))(get_names_and_types_1) in
           debug "\n\nMerging in %a\n\n" print_recur e;
           let o = term ~bind:(bind ~id ~ty:(ext_ty tys) ()) ~pat ~e () in
           debug "\n\nFinal %a\n\n" print o;
           None,[], o
       | _ -> (* no pending pattern, we can safely discard useless pattern *)
           let discarded = List.tl l in
           if discarded <> [] then debug "\n\nDiscarding B immediate useless patterns %a\n\n" (Format.pp_list "|" print) l;
           let case = List.hd l in
           let case = match ty with None -> case | Some ty -> add_ty_in_term ty case in
           None,[],case

       end
  end

(* partition any patterns *)
let extract_any ol =
  List.partition (function
  | _, Term { pat=Any }  -> true
  | _ -> false
  ) ol

(* partition unstrict patterns *)
let extract_unstrict ol =
  List.partition (function
  | _, And { strict=false}  -> true
  | _ -> false
  ) ol

let add_i i l = List.map (fun j->i,j) l
let add_i_fst i l = List.map (fun j->i,fst j) l

(*
  dispatch pattern by class, no specific order on pattern
  should be call on separate pattern group,
  strict, unstrict, any
*)
let rec class_layer ol =
    let hash_add h (k,v) =
      let l = try Hashtbl.find h k with Not_found -> [] in
      Hashtbl.replace h k (v::l)
    in
    let hash_pop h (k,_) =
      try
        let l = Hashtbl.find h k in
        Hashtbl.remove h k;
        Some(k,List.rev l)
      with Not_found -> None
    in
    let class_o = l_map(ol)(fun o->PatternClass.from_pattern o,o) in
    let classes     = Hashtbl.create 6 in
    l_iter(class_o)(hash_add classes);
    let classes_patterns = l_filter_map(class_o)(hash_pop classes) in
    classes_patterns

and simplify_recur recur =
  debug "\n\n--------------------------------------------------------------------------------------Try to simplify %a \n%!" print_recur recur;
  match recur with
  | Recur (_,_, And { fields = [] ; term = { bind = { ident = [] ; coerce_ty = [] } ;  content = c }} )
  | Recur (_,_, Term { pat = Any ; bind = { ident = [] ; coerce_ty = [] } ;  content = c } )
    -> c
  | _ -> recur

and recurse_on new_ident ~path recurse_todo (o:onion) =

    match recurse_todo with
    | [] -> (
        match o with
        | Or { cases = [o] ; default = None } -> o
        | _ -> o
      )

    | (_id,[])::rl -> recurse_on new_ident ~path:(List.tl path) rl o

    | (id, ((field,field_ident,ty) :: fields ) )::rl ->
        debug "Recursion on record %a field %s\n" print_id id field;
        let submatches = recurse_on_field_and_recurse new_ident  ~path:(List.tl path)  rl id field field_ident fields ty o in
        let r = update_First_Term (simplify_recur  (Recur(field_ident,ty,submatches))) o in
        debug "\nA BEFORE %a\nAFTER %a" print o print r;
        r

and recurse_on_field_and_recurse new_ident ~path  recurse_todo id field field_ident other_fields (ty:'a option) o =
  let transform id field _field_ident and_patterns other_fields (ty:  'a option) =
    debug "transform: args %s %a\n" field print (Or {cases=and_patterns;default=None;ty=ty});
    let (field_patterns:onion list) = get_patterns_for_field ty id field and_patterns in
    debug "transform: Patterns %a\n" print (Or {cases=field_patterns;default=None;ty=ty});
    debug "transform: Type extraction TODO\n";
    (* inject the next recursion *)
    let recurse_todo = if other_fields=[]  && recurse_todo=[] then [] else (id,other_fields)::recurse_todo in
    or_cases ~path:(field::path) ~recurse_todo:recurse_todo ty new_ident field_patterns
  in
  (* find the recur node to recurse on *)
  (* Recur(id,Or { cases } ) *)
  let map tra o = match o  with
    | ( Term { content = Recur( _continue_record_ ,_ty, ands_ ) }
      | And  { term = { content = Recur( _continue_record_ ,_ty ,ands_) }})
        when _continue_record_== id ->
        debug "Continue on sub record %a with field %s\n" print_id id field;
          let r =
            begin match ands_ with
            | Or { cases = ands_} -> transform id field field_ident ands_ other_fields ty
            | And _ -> transform id field field_ident [ands_] other_fields ty
            | _ -> (debug "ASSERT FAILURE %a \n%!"print ands_ ; assert false)
            end
          in r
    | _ -> tra o
  in Trav.traverse_map map o

and strictify_record ty nonstrict =
    nonstrict
   |> List.map (fun (i,pat) -> add_i_fst i (PatternClass.strictify_pattern_ty (Option.get ty) pat))
   |> List.flatten
   |> List.partition (fun (_,p) -> PatternClass.is_strict p)

and strictify_const const_pats nonstrict =
  let class_pats = List.map  (fun (_i,pat) -> PatternClass.from_pattern pat ) const_pats in
    nonstrict
   |> List.map (fun (i,pat) ->
        add_i i (List.filter_map (fun class_pat -> PatternClass.strictify_pattern_class class_pat pat) class_pats)
      )
   |> List.flatten


and or_cases ~path ?(recurse_todo=[])  ty new_ident cases =
  let need_to_recurse =
    let rec aux r =  match r with [] -> false | (_,[])::r -> aux r | _ -> true in aux recurse_todo
  in
  let cases = flatten_or cases in
  (* when well tested 1) and 2) should be merged for speed reason *)

  let is_and_cases   = List.for_all (function And _               -> true | Term { pat = Any } -> true | _ -> false) cases in
  let is_const_cases = List.for_all (function Term { pat = Cst _ }-> true | Term { pat = Any } -> true | _ -> false) cases in
  assert(is_const_cases || is_and_cases);

  (* 0) we number patterns to have retrieve the right when remerging separated cases *)
  let cases = List.mapi (fun i p -> (i,p)) cases in


  (* 1) separate patterns in 3 groups, strict, unstrict (contains ...), any *)
  let any,mix = extract_any cases in
  let unstrict,strict = extract_unstrict mix in
  if debug_flag then (
    debug "\n\nStrict    : %a\n\n" (Format.pp_list "|" print) (List.map snd strict);
    debug "\n\nUnstrict  : %a\n\n" (Format.pp_list "|" print) (List.map snd unstrict);
    debug "\n\nAny       : %a\n\n" (Format.pp_list "|" print) (List.map snd any);
  );


  (* 2) pattern completion
     detect incomplete pattern on the first layer 'any's appart,
     if complete then any patterns are discarded,
     if not a 'any' pattern is introduced
  *)
  let any =
    debug "TODO : I should do completion verification here\n";
    (* completion verification *)
    let missing = if is_const_cases then
      let missings= if strict = [] then (assert (any<>[]); [] ) else L.get_missing (List.map (function _,Term {pat = Cst c} -> c | _ -> assert false) strict) in
      if missings <> [] || strict = [] then (
        let exc = Missing_const_case(path,missings) in
        Some exc
      ) else None
    else (
      let ty_classes = L.strictify_record_ty (Option.get ty) [] in
      let ty_classes = l_map(ty_classes)(fun (l,s)->PatternClass.CRECORD l,s) in
      let strict_classes = l_map(strict)(fun (_,p)->PatternClass.from_pattern p) in
      let unstrict_classes = l_map(unstrict)(fun (_,p)->PatternClass.from_pattern p) in
      let remaining_ty_classes = l_fold(ty_classes,[])(fun (class_,_strictness) remainings->
        let ty_class_covered_by_pattern_class =
            List.exists (fun candidate -> PatternClass.is_instance_of          ~candidate ~class_) strict_classes
         || List.exists (fun candidate -> PatternClass.is_unstrict_instance_of ~candidate ~class_) unstrict_classes
        in
        if ty_class_covered_by_pattern_class
        then remainings
        else class_::remainings
      ) in
      debug "remaining classes %a"  (Format.pp_list " ++ " PatternClass.print_class) remaining_ty_classes;
      if remaining_ty_classes <> [] then (
        let remaining_ty_classes = List.sort PatternClass.compare remaining_ty_classes in
        let exc =  Missing_record_class(path,l_map(remaining_ty_classes)(function PatternClass.CRECORD l -> l | _ -> assert false)) in
        Some exc)
      else None
    ) in
    match missing with
    | Some exc ->
        debug "Appart 'any', incomplete pattern\n";
        if any = [] then (
          raise_public exc;
          debug "Adding failure branch\n";
          let buf = Buffer.create 10 in
          let fmt = Format.formatter_of_buffer buf in
          print_public_exc fmt exc;
          Format.fprintf fmt "@\n on path %a@\n" (Format.pp_list "." Format.pp_print_string) (List.rev path);
          Format.pp_print_flush fmt ();
          let str = Buffer.contents buf in
          [max_int,term ~e:(Failure str) ()]
        ) else any
    | None -> any
  in
  (* when any is alone, it should not be strictified, otherwise you can make infinite pattern for recursives types *)
  let any_need_to_be_strictified = not (any<>[] && strict = [] && unstrict = []) in

  (* 3) make groups independant, and eventually eliminate the 2 weak groups, done by strictification *)
  (* let strictify = if ty = None then PatternClass.strictify *)
  let strictified_record,unstrict = strictify_record ty unstrict in
  let strict = strict@strictified_record in
  let strictified_any,any =
    if any_need_to_be_strictified
    then (
      if is_and_cases then (
        debug "Strictify as Record";
        let strict,unstrict = strictify_record ty any in
        strict, unstrict@(if L.is_open_ty ty then any else [])
      ) else (
        debug "Strictify as Const";
        strictify_const strict any,any
      )
    ) else [],any
  in
  let strict = strict@strictified_any in

  (* we reorder in each group to get back the initial semantic *)
  let strict = List.sort Pervasives.compare strict |> List.map snd in
  let unstrict = List.sort Pervasives.compare unstrict |> List.map snd in
  let any = List.sort Pervasives.compare any |> List.map snd in

  if debug_flag then (
    debug "\n\nFINAL Strictified Any %a\n\n" (Format.pp_list "|" print) (List.map snd strictified_any);
    debug "\n\nFINAL Strictified Unstrict %a\n\n" (Format.pp_list "|" print) (List.map snd strictified_record);

    debug "\n\nFINAL Strict %a\n\n" (Format.pp_list "|" print) strict;
    debug "\n\nFINAL Unstrict %a\n\n" (Format.pp_list "|" print) unstrict;
    debug "\n\nFINAL Any %a\n\n" (Format.pp_list "|" print) any;
  );

  (* 4) separate patterns by class (shape of the first layer in the pattern *)
  let strict_classes_patterns   = class_layer strict in
  let unstrict_classes_patterns = class_layer unstrict in
  let any_class_patterns        = class_layer any in


  (* 5) merge pattern of the same class if possible *)
  let class_merge = PatternClass.class_merge new_ident need_to_recurse in
  let strict_sub_matches   = l_map(strict_classes_patterns  )(class_merge true ty ) in
  let unstrict_sub_matches = l_map(unstrict_classes_patterns)(class_merge false ty) in
  let any_sub_matches      = l_map(any_class_patterns       )(class_merge false ty) in

  (* 6) finalise each class, by recursively and jointly apply the normalisation
     TODO move outside
  *)
  let finalise_class (recursion,subpatterns,pattern) =
    (* recurse on Recur nodes *)
    (* non empty list means that a reference record name need to be created *)
    let merge_pat = if subpatterns=[] then pattern else Or {cases=subpatterns ; default=None ;ty=ty} in

    match recursion with
    | Some((id,fl) as recurs) ->
        (* asking for a joint matching on record id *)
        debug "Adding joint matching on record %a\n" print_id id;
        let recurs_node = Recur(id,ty, (*normalize new_ident*) merge_pat) in
        if fl = [] then recurse_on new_ident ~path recurse_todo merge_pat
          (* refuse to add a recursion that will be never taken care of
             see recurse_on
          *)
        else
          let (o:onion) = update_First_Term recurs_node pattern  in
          ((recurse_on new_ident ~path:("dummy_field"::path) (recurs::recurse_todo) o):onion)

    | None ->
        match recurse_todo with
        | [] ->
            (* end of matching *)
            if debug_flag then (
              debug "End of matching with %a\n" print pattern;
              if List.length subpatterns > 1 then debug "\n\nDiscarding finally useless patterns %a\n\n" (Format.pp_list "|" print) (List.tl subpatterns)
            );
            if subpatterns=[] then pattern else List.hd subpatterns

        | _  -> recurse_on new_ident ~path recurse_todo merge_pat
  in
  let strict_sub_matches   = l_map(strict_sub_matches  )(finalise_class) in
  let unstrict_sub_matches = l_map(unstrict_sub_matches)(finalise_class) in
  let any_sub_matches      = l_map(any_sub_matches     )(finalise_class) in

  let cases = strict_sub_matches @ unstrict_sub_matches @ any_sub_matches in
  let result =  match cases with
    | [] -> assert false
    | [o] -> o
    | _ -> Or { cases = cases ; default = None ; ty=ty}
  in
  debug " or_cases result %a@\n%!" print result;
  result

and normalize new_ident o =
    (* should first collect all expression and all initial bind *)
    debug "Normalize : @\n%a @\n" print o;
    match o with
      (* already normalised form ? *)
    | Or {cases=l1 ; default=Some e; ty=ty} -> normalize new_ident (Or {cases=l1@[Term e]; default = None ; ty=ty})

    | Or {cases=l1 ; default=None  ; ty=ty} ->
        assert(l1<>[]);
        or_cases ~path:[] ty new_ident l1

    | _ ->
        Format.printf "\n%a\n" print o;
        assert false


let normalize new_ident o =
  if !ref_public_exception != [] then
    failwith "QmlPatternAnalysis.normalize, exception stack was not flushed";
  incr nb_total;
    let r =
      try
        Printexc.record_backtrace true;
        normalize new_ident o;
      with ex -> (
        Printexc.print_backtrace stdout;
        incr nb_failure;
        flush_exceptions_stdout ();
        debug "ONION nb failures  : %d / %d @\n %!"  !nb_failure !nb_total;
        raise ex
      );
    in
    r

end

end

module QmlOnion
=
struct

  (**
     to have access to gamma in Lang, we use a dirty ref instead of horribles functors :)
     the ref can only be initialize once
     the ref cannot be used uninitialized
  *)
  type typer_env = {
    gamma : QmlTypes.gamma ;
    annotmap : QmlAst.annotmap ;
  }

  let typer_env, typer_env_initialize, typer_env_reset =
    let r = ref (None : typer_env option) in
    (fun () ->
       match !r with
       | None -> assert false
       | Some env -> env),
    (fun env -> r := Some env),
    (fun () -> r := None)

  module Private  = struct
  let raw_get_field_type field_ty_l field=
      try List.assoc field field_ty_l
      with Not_found -> failwith (Printf.sprintf "Not found %s in %s"  field (String.concat "," (List.map fst field_ty_l)))

  let has_strictly_fields fields ty =
      match ty with
      | Q.TypeRecord( Q.TyRow(field_ty_l,_)) -> List.for_all (fun (f,_) -> List.mem f fields) field_ty_l
      |  _ -> assert false

  let filter_ambiguous fields lty =
      match lty with
      |  _::_::_ -> List.filter (has_strictly_fields fields) lty (* SLOW : but needed for types like with repeating fields, like OpaType.ty *)
      | _ -> lty

  end

  (** contains the minimal set of function to be defined for using Onion functor *)
  module Lang : QmlPatternAnalysisSig.ONIONLANG
    with
        type ident = Q.ident
    and type const = Q.const_expr
    and type expr  = Q.expr
    and type ty    = Q.ty
    =
  struct
    type ident = Q.ident
    type const = Q.const_expr
    type expr  = Q.expr
    type ty    = Q.ty

    let gen_ident ?ident field =
      match ident with
      | Some id -> Ident.refresh ~map:(fun s-> s^"_onion_"^field) id
      | None -> Ident.next ~descr:"Onion" field

    let print_id f i = Format.pp_print_string f (Ident.to_string i)
    let print_ty    = QmlPrint.pp#ty
    let print_expr  = QmlPrint.pp#expr
    let print_const f c = Format.pp_print_string f (Q.Const.string_of_expr c)
    let compare_const = QmlAstUtils.Const.compare

(** compute missings constants,
    gives interesting hints like missing int between successive ints ...
    for string and float, it's just a joke
*)
    let get_missing l =
      let l = List.sort compare_const l |> List.uniq ~cmp:compare_const in
      let between v v'= match v,v' with
      | Q.Int i, Q.Int i' -> if i+1=i' then [] else [Q.Int( i+1 )]
      | Q.Float f, Q.Float f' -> let f'' = (f+.f')/.2.0 in if f<f'' && f''< f' then [Q.Float f''] else []
      | Q.String s, Q.String s' -> [Q.String (s^"_"^s')]
      | Q.Char c, Q.Char c' ->
          let c = Char.code c in
          let c' = Char.code c' in
          if c+1=c' then [] else [Q.Char( Char.chr (c+1))]
      | Q.Char _, _ | Q.String _ , _ | Q.Int _ , _ | Q.Float _,_ -> assert false
      in
      let outside first last = match first,last with
      | Q.Int i, Q.Int i' ->
          if i= min_int then
            if i'= max_int then [ Q.String "Nothing" ]
            else [Q.Int (i'+1)]
          else [Q.Int (i-1)]
      | Q.Float f, Q.Float f' ->
          if f = -. infinity then
            if f' = infinity then [ Q.String "Nothing" ]
            else [Q.Float (f +. 1.0)]
          else [Q.Float (f' -. 1.0)]
      | Q.String _, Q.String s' -> [ Q.String (s'^", me and many many others") ]
      | Q.Char c, Q.Char c' ->
          let c = Char.code c in
          let c' = Char.code c' in
          if c = 0 then
            if c' = 255 then []
            else [Q.Char (Char.chr (c'+1))]
          else [Q.Char (Char.chr (c'-1))]
      | Q.Char _, _ | Q.String _ , _ | Q.Int _ , _ | Q.Float _,_ ->  assert false
      in
      let first = List.hd l in
      let rec aux last l =
        match last,l with
        | _ , [] -> outside first last
        | v , last::rl  -> (between v last)@(aux last rl)
      in aux first (List.tl l) |> List.rev



    let rec get_type_cases ty fields =
      let row _unstrict (fields:string list) (field_ty:(string*ty) list)  :ty list =
        (* SLOW : see if it matters *)

        assert(ordered2 (List.sort compare field_ty));
        assert(ordered (List.sort compare fields));
        try
          let fields_of_type = l_map_sort(field_ty)(fst) in
          let field_type_from_fields =  l_map_sort(fields)(fun f->f,Private.raw_get_field_type field_ty f) in
          match fields_completion (Private.raw_get_field_type field_ty) fields_of_type field_type_from_fields [] with
          | Some fields ->
              let fields = List.sort compare fields in
              assert(ordered2 fields);
              [Q.TypeRecord(Q.TyRow(fields,_unstrict) )]
          | None -> []
        with Failure _ -> []
      in
      let rec self ty fields =
        match ty with
          (* trivial cases *)
        | Q.TypeVar   _
        | Q.TypeConst _
        | Q.TypeArrow _
        | Q.TypeAbstract _
          -> []

        (* border line cases *)
        | Q.TypeForall(_,_,_,ty) -> self ty fields

        (* gamma aware cases *)
        | Q.TypeName(params, ident) ->
          let gamma = (typer_env ()).gamma in
          let ty = QmlTypesUtils.Inspect.find_and_specialize gamma ident params in
          self ty fields

        (* interesting cases *)
        | Q.TypeRecord( Q.TyRow(field_ty_l,_unstrict) ) ->
(*            assert( _unstrict=None ); *)
            row _unstrict fields field_ty_l

        | Q.TypeSum( Q.TyCol(row_l,_unstrict) ) -> l_map_flat(row_l)(row  None fields)

        | Q.TypeSumSugar ty_l -> l_map_flat(ty_l)(fun ty->self ty fields)


      in self ty fields

    let strictify_record_ty (ty:ty) (fields:string list) =
      debug "Strictify record %a {%a} @\n%!" print_ty ty (Format.pp_list "," Format.pp_print_string) fields;
      List.map (function
        | Q.TypeRecord( Q.TyRow(field_ty_l,unstrict) ) ->
            debug "Sol = <<%a>> @\n%!"  (Format.pp_list "," Format.pp_print_string) (List.map fst field_ty_l);
            let strict =
              if unstrict = None
              then `closed
              else `open_
            in
            l_map_sort(field_ty_l)(fun (f,_)->f), strict
        | _ -> assert false
      ) (get_type_cases ty fields)

    (* SLOW TODO : add a strict get_type_cases for better perfs *)
    let strict_get_field_type (ty:ty) (fields:string list) (field:string):ty=
      match Private.filter_ambiguous fields (get_type_cases ty fields) with
      | [ Q.TypeRecord( Q.TyRow(field_ty_l,_)) ] -> Private.raw_get_field_type field_ty_l field
      | tys  ->
          debug "get_field_type %a {%a} %s    in <<%a>>@\n%!" print_ty ty (Format.pp_list "/" Format.pp_print_string) fields field (Format.pp_list "," print_ty) tys ;
          (* typer specification make this case an assert false case *)
          assert false

    let is_open_ty ty =
      match ty with
      | Some (Q.TypeSum( Q.TyCol(_,Some _ ) )) -> true
      | _ -> false

  end

  module QmlOnion = Onion(Lang)
  include QmlOnion

  type qml_onion = QmlOnion.onion

  module QC = QmlAstCons.UntypedExpr

  let cons = QmlAstCons.untyped_cons

  let remove_boring_names o =


    (* get all one layers alias *)
    let add_list acc l =
      match l with
      | x::_ -> (x,l)::acc
      | _ -> acc
    in
    let rec collect_alias_and_fields_alias acc o =
      match o with
      | Term { bind = { ident = l} } -> add_list acc l
      | And  { fields=fields ; term = { bind = { ident = l } } } ->
          l_fold(fields,add_list acc l)(fun (_,o) acc->
            collect_alias_and_fields_alias acc o
          )
      | _ -> acc
    in

    (* create substitution and counting map *)
    let update_alias_maps ((_count_map,_alias_map) as acc) o =
      debug "COLLECT ON %a@\n%!" print o;
      let aliases = collect_alias_and_fields_alias [] o in
      l_fold(aliases,acc)(fun (main_name,others) (count_map,alias_map) ->
        let reference = ref 0 in
        let accessor()=
          debug "ACCESSING %a@\n%!" print_id main_name;
          incr reference;QC.ident main_name
        in
        IdentMap.add main_name (ref 0) count_map,
        l_fold(others, alias_map)(fun alias alias_map->
          debug "ADDING %a@\n%!" print_id alias;
          IdentMap.add alias accessor alias_map
        )
      )
    in

    let change_term_ident term ident =
      { term with
        bind = { term.bind with ident = ident }
      } in
    let change_ident o ident =
      match o with
      | Term term ->
          Term (change_term_ident term ident)
      | And( { term = term } as and_ ) ->
          And {and_ with term = change_term_ident term ident}
      | _ -> assert false
    in
    let subs (_count_map,alias_map) o =
      match o with
      | Term ({ content = Expr e } as term) ->
          Term { term with
            content = Expr (QmlAstUtils.ExprIdent.substitute alias_map e) }

      | And ({ term = { content = Expr e } as term} as and_)  ->
          And { and_ with term = { term with
            content = Expr (QmlAstUtils.ExprIdent.substitute alias_map e) } }

      | Term { content = Recur(id,_,_) }
      | And { term = { content = Recur(id,_,_) } } ->
          (try ignore((IdentMap.find id alias_map)()) with Not_found ->
            (*debug "NOT FOUND %a@\n%a\n---------------\n%a\n%!" print_id id print original print o; assert false*)
            () (* this is currently working but in a bad shape, see coll_and_subs_and_rm *)
          ); (* counting the use of the id *)
          o

      | _ -> o
    in
    let rm (count_map,_) o =
      match o with
      | Term { bind = { ident = x::_ } }
      | And  { term = { bind = { ident = x::_ } } } ->
          change_ident o
            (if true || !(IdentMap.find x count_map) > 0 then [x] else (debug "REMOVING %a@\n" print_id x ; []))
      | _ -> o
    in
    let coll_and_subs_and_rm env o =
      (* do something better,
           can quadratic here, since substitution is apply hollisticly
           the count is not accurate, just distinguate between used and not used
         = > use a physical cache for instance
      *)

      let env = update_alias_maps env o in
      let o = Trav.map (subs env) o in (* BAD HERE *)
      let o = rm env o in
      env,o
    in
    snd(Trav.foldmap_down coll_and_subs_and_rm (IdentMap.empty,IdentMap.empty) o)




  let of_qml_patt_X ty names tys l =
    assert( ty <> None );
    let rec aux_record names tys pat e =
      match pat with
      | Q.PatRecord (_, fields, rowvar) ->
          let map (field, pat) =
            let pat = aux_term_and [] [] (pat, End) in
            (field, pat)
          in
          let fields = List.rev_map map fields in
          let fields = List.sort Pervasives.compare fields in
          And {
            fields ;
            strict = rowvar = `closed ;
            term = {
              bind = bind ~id:names ~ty:tys ();
              pat = Any;
              content = e ;
            }
          }

      (* impossible cases *)
      | Q.PatAny _ -> assert false
      | Q.PatCoerce _ -> assert false
      | Q.PatConst _  -> assert false
      | Q.PatVar _  -> assert false
      | Q.PatAs _ -> assert false

    and aux_term_and names tys (pat, e) =
      match pat with
      | Q.PatAny _                        -> term ~bind:(bind ~ty:tys ())   ~e ()
      | Q.PatVar (_, ident)               -> term ~bind:(bind ~id:[ident] ~ty:tys ()) ~e ()
      | Q.PatConst (_, c)                 -> term ~bind:(bind ~ty:tys ()) ~pat:(Cst c) ~e ()
      | Q.PatRecord _                     -> aux_record names tys pat e
      | Q.PatCoerce (_, pat, coerce_ty)   -> aux_term_and names (coerce_ty::tys) (pat,e)
      | Q.PatAs (_, pat, ident) ->
          let term = aux_term_and names tys (pat, e) in
          add_bind_onion ident term

    and aux_or ty l names tys =
      match List.map (fun (pat,e)-> aux_term_and names tys (pat,(Expr e))) l with
(*      | x :: [] -> x *)
      | l -> Or { cases = l ; default= None ; ty = ty}
    in
    aux_or ty l names tys

  let of_qml_patt_expr ty l : qml_onion = of_qml_patt_X ty [] [] l

  (** only working on normalized code *)
  let rec to_qml_patt_X annotmap ~annot name o =
    (* generating sub match recursively
       propagating aliases to the final expression
    *)
    (* let pos = QmlAnnotMap.find_position annot annotmap in *)
    (* let annot = Annot.next () in *)
    (* let annotmap = QmlAnnotMap.add_position annot pos annotmap in *)

    let coerce pat tys = l_fold(tys,pat)(fun ty pat -> QC.patcoerce pat ty) in

    let rec aux_recur r =
      match r with
      | Expr e -> Some e
      | Failure str->
          let str= QC.string ("Match failure, detected at compile time see compilation warnings,\n please check missing case in pattern : "^str) in
          let str = Q.QAnnot.New.expr str annot in
          let failure = QC.directive `fail [ str ] [] in
          (*
            FIXME: this duplicates the annotation
            Because annotation map is not here
                Need to use the object constructor to solve
          *)
          let failure = Q.QAnnot.New.expr failure annot in
          Some failure
      | Recur (ident, _, o) ->
          let (names,_tys),el = top_level name o in
          let ident = QC.ident ident in
          let r= QC.letin ( List.map (fun n-> n,ident) names) (QC.match_ ident el) in
          Some(r) (* cons#match_ cons#ident_with_annot *)
      | End _  -> None


    (* todo, adding global position of the pattern matching *)
    and aux_and name { fields=fields ; strict=strict ; term=term } =
      let fields = l_map(fields)(fun (f,o)->
        match aux name o with
        | pat,None -> (f,pat)
        | pat , Some _  -> debug "\nASSERT FAILURE %a\n%!" print o ; (*assert false*) f,pat
      ) in
      coerce (if strict then QC.patrecord fields else QC.patextendrecord fields) term.bind.coerce_ty,
      aux_recur term.content

    and aux name o =
      match o with
      | Or _ ->
          (* only possible at top level after normalisation *)
          debug "\nASSERT FAILURE %a\n%!" print o ;
          assert false

      | And and_ ->  aux_and name and_

      | Term { bind = { ident= ids ; coerce_ty = tys } ; pat = Any  ; content = content } ->
          let pat = match ids with
          | x::rl ->
              assert( rl = []); QC.patvar x
          | _     -> QC.patany ()
          in
          coerce pat tys, aux_recur content

      | Term { bind = { ident= _ ; coerce_ty = tys } ; pat = Cst c  ; content = content } ->
          coerce (QC.patconst c) tys, aux_recur content

    and top_level name o  =
      try
        (* collect all aliases, constructing a substitution map *)
        (* collect all expression *)
        let (names,tys),l = match o with
          | Or { cases = cases ; default = default } ->
              let cases = Option.default_map cases  (fun x-> cases@[Term x]) default in
              List.foldl Normalize.get_names_and_types_1 cases ([],[])
              ,List.map (aux name) cases

          | _ -> Normalize.get_names_and_types_1 o ([],[]),[aux name o]
        in
        let names = names |> List.sort Pervasives.compare
                          |> List.uniq ~cmp:Pervasives.compare in
        (names,tys),l_map(l)(fun (pat,e) -> pat,Option.get e)
      with e -> (
        Format.printf "@\n FAULTY : %a @\n %!" print o ;
        raise e
      )
    in
    debug "@\n BEFORE : %a @\n %!" print o ;
    let o = remove_boring_names o in
    debug "@\n CLEANED : %a @\n %!" print o ;
    let name_ty_s,el = top_level name o in
    debug "@\n FINAL : %a @\n %!" print_expr (QC.match_ (QC.record []) el);
    (*let change_pos e = {e with annot = annot} in *)
    annotmap,name_ty_s, el

end

module Transform = struct

  let optimize annotmap ~expr _ty _funname l =
    (* Format.printf "----------------------------------@\n"; *)
    let annot = Q.QAnnot.expr expr in
    let o =  QmlOnion.of_qml_patt_expr _ty l in
    (*    Format.printf "VANILLA %a@\n@\n%!" QmlOnion.print o; *)
    let next = (fun s -> Ident.refresh ~map:(fun _ -> "field_"^s ) _funname) in
    let no =
      try QmlOnion.Normalize.normalize next o
      with QmlOnion.Normalize.Exc(e) as ex ->
        (Format.printf "\nEXCEPTION %a\n" (Format.pp_list ";" QmlOnion.Normalize.print_exc) e;  raise ex )
    in
    let _ = if QmlOnion.Normalize.has_public_exceptions () then (
      let ctx = QmlError.Context.expr expr in
      QmlError.warning ~wclass:WarningClass.pattern ctx "%a" QmlOnion.Normalize.flush_exceptions_fmt ()
    ) else ()
    in

    (*    Format.printf "NORMALIZED %a@\n@\n%!" QmlOnion.print no; *)
    let annotmap,name_ty_s, l = QmlOnion.to_qml_patt_X annotmap ~annot next no in
    annotmap,name_ty_s,l

end



(* EXPORTED VALUES *)

module Env =
struct
  let reset = QmlOnion.typer_env_reset
end

type pattern_matching = {
  env : QmlOnion.typer_env ;
  match_label : Annot.label ;
  matched_expr : QmlAst.expr ;
  onion : QmlOnion.qml_onion ;
}

let conversion ~gamma ~annotmap ~label ~matched_expr:expr ~patterns =
  let env = { QmlOnion.gamma ; annotmap } in
  QmlOnion.typer_env_initialize env ;
  let annot = QmlAst.QAnnot.expr expr in
  let ty = QmlAnnotMap.find_ty annot annotmap in
  let onion = QmlOnion.of_qml_patt_expr (Some ty) patterns in
  {
    env ;
    match_label = label ;
    matched_expr = expr ;
    onion ;
  }

type normalized_pattern_matching = pattern_matching

let normalize pm =
  let next s = Ident.next s in
  let onion = QmlOnion.Normalize.normalize next pm.onion in
  { pm with
      onion
  }

module QC = QmlAstCons.TypedExpr

let generation npm =
  let annotmap = npm.env.QmlOnion.annotmap in
  let next s = Ident.next s in
  let onion = npm.onion in
  let matched_expr = npm.matched_expr in
  let annot = QmlAst.QAnnot.expr matched_expr in
  let annotmap, as_bindings, pat_expr =
    QmlOnion.to_qml_patt_X annotmap ~annot next onion in
  let ids, _tys = as_bindings in
  (*
    Rebind identifiers introduced with 'as'.
  *)
  let pattern_matching =
    QmlAst.Match (npm.match_label, matched_expr, pat_expr)
  in
  let annotmap, pattern_matching =
    let fold (annotmap, expr) id =
      let annotmap, matched_expr = QC.copy annotmap matched_expr in
      QC.letin annotmap [ id, matched_expr ] expr in
    (* try *)
      List.fold_left fold (annotmap, pattern_matching) ids
    (* with *)
    (* | Invalid_argument "List.fold_left2" -> assert false *)
  in
  annotmap, pattern_matching

let has_public_exceptions = QmlOnion.Normalize.has_public_exceptions
let flush_exceptions_fmt = QmlOnion.Normalize.flush_exceptions_fmt
