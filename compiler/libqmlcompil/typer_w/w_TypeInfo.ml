let flag = true (*for measuring time*)

type obj = W_Algebra.simple_type_desc
type dir_info = QmlAst.qml_directive * Annot.label
type env_info = Ident.t * Annot.label
type exp_info = Annot.label

type info =
  | FromEnv of env_info
  | Directive of dir_info
  | Location of Annot.label
  | NoInfo of string
  | Link of obj
  | Exception

let cmp_info i1 i2 =
  match (i1, i2) with
   | (Location l1, Location l2) ->
      W_Misc.cmp_pos (Annot.pos l1) (Annot.pos l2)
   | (Link l1, Link l2) -> compare l1 l2
   | (FromEnv i1, FromEnv i2) -> compare i1 i2
   | (Directive (i1, _), Directive (i2, _)) -> compare i1 i2
   | _                  -> -1


let sameTy x y = x == y

let findAll x ls =
  let rec aux x ls ack =
   match ls with
    | [] -> ack
    | (y, i)::lss ->
      if sameTy x y then aux x lss (i::ack) else aux x lss ack
  in aux x ls []

exception NoLoc

let rec findFirstLoc = function
 | []               -> raise NoLoc
 | (Location l::_) -> Location l
 | _::ls            -> findFirstLoc ls

let getNextInfo ls =
  try (findFirstLoc ls, [])
  with NoLoc ->
     match ls with
      | []   -> raise Not_found
      | i::is -> (i, is)

let rec belongs x = function
 | [] -> false
 | y::ys -> if sameTy y x then true else belongs x ys

let rec __retrieve x ti old rest all=
  try
   match getNextInfo rest with
    | (Link x', rest')   ->
       if belongs x' old
        then __retrieve x ti old rest' all
        else __retrieve x' ti (x::old) (List.append rest' (findAll x' ti)) all
    | (Location l, _ ) -> Location l
    | (NoInfo _, rest' ) ->  __retrieve x ti old rest' all

    | (otherwise, rest') -> __retrieve x ti old rest' (otherwise::all)
  with
   Not_found -> match all with
                | [] -> NoInfo "not_found"
                | i::_ -> i

let _retrieve x ti old rest = __retrieve x ti old rest []

let infoState = ref []

let take_subterms = W_SubTerms.take_subterms

let add_linked_object o link =
  if flag then infoState := (o, Link link)::!infoState else ()

let add_expn_object o =
  if flag then infoState := (o, Exception)::!infoState else ()

let add_loc_object o loc =
  if flag then infoState := (o, Location loc)::!infoState else ()

let add_env_object o e =
  if flag then infoState := (o, FromEnv e)::!infoState else ()

let add_dir_object o e =
  if flag then infoState := (o, Directive e)::!infoState else ()

let addrec_loc_object o loc =
 if flag then (
  let subs = take_subterms o in
  let foo = List.map (fun t -> (t, Location loc)) subs in
  infoState := List.append foo !infoState
 ) else ()

let addrec_linked_object o link =
  if flag
   then
    infoState := List.append
     (List.map (fun t -> (t, Link link)) (take_subterms o))
     !infoState
   else ()

let addrec_expn_object o =
  if flag
   then
     infoState := List.append
       (List.map (fun t -> (t, Exception)) (take_subterms o))
       !infoState
   else ()

let addrec_env_object o link =
  if flag
   then
    infoState := List.append
      (List.map (fun t ->  (t, FromEnv link)) (take_subterms o))
      !infoState
    else ()

let addrec_dir_object o link =
 if flag then
  infoState := List.append
    (List.map (fun t -> (t, Directive link)) (take_subterms o))
    !infoState
 else ()

let add_no_object o s =
  if flag then
   infoState := (o, NoInfo s)::!infoState
  else ()

let retrieve (x : obj)
  = _retrieve x !infoState [] (findAll x !infoState)

let clean_type_info _ =
  infoState := []

