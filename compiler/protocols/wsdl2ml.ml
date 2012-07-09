(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(** wsdl2ml:
    Convert WSDL file into OCaml support code.

    - Currently:
      % wsdl2ml <wsdl file>

    - Types generated in <wsdl file>Types.ml
*)
let printf = Printf.printf
let eprintf = Printf.eprintf
let sprintf = Printf.sprintf
let fprintf = Printf.fprintf
let kfprintf = Format.kfprintf
let ikfprintf = Format.ikfprintf
module List = Base.List
module String = Base.String
module Char = Base.Char
module O = Ocaml
module Cons = O.Cons
module WC = Wsdl2mlCommon

let debug = ref true
let verbose = ref true
let myname = ref "!\"£$%^&*()" (* Would you use this as a type name? *)
let dname = ref ""
let watchme = ref ""
let collect = ref false
let post_headers = ref true
let mlidl = ref false

let dprintf fmt =
  (if !debug then kfprintf else ikfprintf)
    (fun _ -> Format.pp_print_flush Format.err_formatter ()) Format.err_formatter fmt

let nprintf fmt =
  (if !debug && (!dname = !myname || !myname = "") then kfprintf else ikfprintf)
    (fun _ -> Format.pp_print_flush Format.err_formatter ()) Format.err_formatter fmt

let os o = Option.to_string (fun s -> s) o
let ot = function Some x -> x | None -> "_"

(* We can't use Req here because of licence probems, that code
 * was taken from the internet.  We also can't use Http_client
 * because of the ridiculous dependencies between protocols and
 * libnet.  We would have to write something completely new.
 *)
let geturl url =
  raise (Failure "geturl: Not implemented")
  (*let _, _, content = Req.make_request ~url () in
  content*)

(*let get_tree_url url = WC.get_tree_string (geturl url)*)

let rec resolve_imports (ns,(dtd,tree)) =
  raise (Failure "resolve_imports: Not implemented")
  (*let rec aux imps = function
    | WC.E (((_,"import"),atts) as name, trees) ->
        (*eprintf "import\n%!";*)
        let imps2 =
          match WC.find_att ("","namespace") atts, WC.find_att ("","location") atts with
          | Some ns2, Some loc ->
              (*eprintf "namespace: %s\nlocation: %s\n%!" ns2 loc;*)
              (try
                 let t = get_tree_url loc in
                 (*eprintf "Tree:\n%s\n%!" (string_of_tree t);*)
                 resolve_imports (ns2,t)
               with Failure _ ->
                 eprintf "Can't read URL %s\n%!" loc; [])
          | _ ->
              ((*eprintf "namespace or location missing\n%!";*) [])
        in
        (imps@imps2, WC.E (name, trees))
    | WC.E (tag, trees) ->
        (*eprintf "tag: %s\n%!" (stag tag);*)
        let imps2, trees =
          List.fold_left (fun (imps2, trees) tree ->
                            let imps3, tree = aux [] tree in
                            (imps2@imps3, trees@[tree])) ([],[]) trees in
        (imps@imps2, WC.E (tag, trees))
    | WC.D str ->
        (*eprintf "data: %s\n%!" str;*)
        (imps, WC.D str)
  in
  let imports, tree = aux [] tree in
  (ns,(dtd, tree))::imports*)

(* We actually look for schemas so that we can read XSD files as well... *)
let find_types_ff _types = function
  (*| WC.E (((_,"types"),_), trees) -> _types@trees*)
  | (WC.E (((_,"schema"),_), _)) as tree -> _types@[tree]
  | WC.E _ -> _types
  | WC.D _ -> _types

let ste te =
  let b = Buffer.create 1024 in
  OcamlPrint.Buf.type_expr b te;
  Buffer.contents b

let recolon = Str.regexp_string ":"

let tvcnt = ref 0
let tvar () = incr tvcnt; string_of_int (!tvcnt)

let tvstr = function
  | [] -> ""
  | [tv] -> tv^" "
  | tvs -> (String.sconcat ~left:"(" ~right:")" "," tvs)^" "

type idx_type = Idx | Noidx

module TypeOrder : (OrderedTypeSig.S with type t = string list * string) =
struct
  type t = string list * string
  let compare = Pervasives.compare
end
module TypeMap = BaseMap.Make (TypeOrder)
type 'a typemap = 'a TypeMap.t

let string_of_tn = function
  | ([], tn) -> tn
  | ([tv], tn) -> tv^" "^tn
  | (tvs, tn) -> (String.sconcat ~left:"(" ~right:")" ", " tvs)^" "^tn

let tuple_of_tes = function
  | [] -> "()"
  | [te] -> ste te
  | tes -> String.sconcat ~left:"(" ~right:")" " * " (List.map ste tes)

type ctxt = { mutable first : bool;
              mutable cn : int;
              tnames : StringSet.t;
              ctmap : ((O.type_expr * int) list) StringMap.t;
              typmap : (bool * bool * string * string * string option * string option * O.type_expr) typemap;
              it : idx_type;
              ind : int;
              type_t : string;
            }

let print_typmap ctxt =
  eprintf "typmap:\n";
  TypeMap.iter (fun tn (has_con,isel,name,iname,_mino,_maxo,tt) ->
                  eprintf "%s => %s, %s, %b, %b, %s\n%!" (string_of_tn tn) name iname has_con isel (ste tt)) ctxt.typmap

let get_idx idmap _type =
  try
    (idmap, List.assoc _type idmap)
  with Not_found ->
    let idx = List.length idmap in
    (((_type,idx)::idmap), idx)

let sidmap idmap = String.sconcat ~left:"[" ~right:"]" "; " (List.map (fun (t,i) -> sprintf "(%s,%d)" (ste t) i) idmap)

let rec inslst a l =
  let rec aux = function
    | [] -> [a]
    | (h::t) as l -> if h = a then l else h::(inslst a t)
  in
  aux l

let single_type ctxt name t =
  eprintf "single_type: name=%s t=%s\n%!" name (ste t);
  let idmap = try StringMap.find name ctxt.ctmap with Not_found -> [] in
  let teo = List.assoc_opt t idmap in
  match teo with
  | Some idx -> ctxt, false
  | None -> { ctxt with ctmap = StringMap.add name ((t,0)::idmap) ctxt.ctmap }, true

let equalise_types ctxt name t1 t2 =
  nprintf "equalise_types: name=%s t1=%s t2=%s\n%!" name (ste t1) (ste t2);
  let idmap = try StringMap.find name ctxt.ctmap with Not_found -> [] in
  let t1eo = List.assoc_opt t1 idmap in
  let t2eo = List.assoc_opt t2 idmap in
  let idmap = idmap@(match t1eo, t2eo with
                     | (Some idx1,Some idx2) ->
                         if idx1 <> idx2 then eprintf "Warning: non-equal types (%s,%s)\n%!" (ste t1) (ste t2);
                         []
                     | (Some idx1,None) -> [(t2,idx1)]
                     | (None, Some idx2) -> [(t1,idx2)]
                     | (None, None) -> [(t1,0); (t2,0)]) in
  { ctxt with ctmap = StringMap.add name idmap ctxt.ctmap }

let pidmap from ctxt name =
  let idmap = try StringMap.find name ctxt.ctmap with Not_found -> [] in
  nprintf "%s: idmap(%s)=%s\n%!" from name (sidmap idmap)

let tn_prefix = "t"
let tnplen = String.length tn_prefix + 1
let cn_prefix = "C"
let cnplen = String.length cn_prefix + 1
let cc_prefix = "CC"
let ccplen = String.length cc_prefix + 1

let get_ct from bump prev prefix ctxt tvs name _type =
  nprintf "get_ct(%s):\n%!" from;
  nprintf "  name=%s\n  _type=%s\n%!" name (ste _type);
  match ctxt.it with
  | Idx ->
      let idmap = try StringMap.find name ctxt.ctmap with Not_found -> [] in
      let tn = O.TypeName (tvs, [tn_prefix^"_"^name]) in
      let idmap, idx =
        match List.assoc_opt tn idmap with
        | Some idx -> (inslst (_type,idx) idmap, idx)
        | None -> get_idx idmap _type
      in
      let idxstr = if idx > 0 then "_"^(string_of_int idx) else "" in
      let iname = name^idxstr in
      let pname = (prefix^"_"^iname) in
      nprintf "  -> %s\n%!" pname;
      pidmap "  idmap=" ctxt name;
      ({ ctxt with ctmap = StringMap.add name idmap ctxt.ctmap }, iname, pname)
  | Noidx -> (ctxt, name, (prefix^"_"^name))

let get_typename ?(bump=true) ?(prev=false) from ctxt tvs name _type =
  get_ct (from^"->get_typename") bump prev tn_prefix ctxt tvs name _type

let get_consname ?(bump=true) ?(prev=false) from ctxt tvs name _type =
  get_ct (from^"->get_consname") bump prev cn_prefix ctxt tvs name _type

(*let get_tconsname ?(bump=true) ?(prev=false) ctxt tvs name _type =
  get_ct (from^"->get_tconsname") "get_tconsname" bump prev cc_prefix ctxt tvs name _type*)

(* Really corny, we have to do something about this... *)
let tn2cn new_pfx = function
  | O.TypeName (tvs, tn) ->
      sprintf "%s %s" (tvstr (List.map ste tvs))
                      (Str.replace_first (Str.regexp_string tn_prefix) new_pfx (List.last tn))
  | te -> raise (Failure (sprintf "tn2cn: %s" (ste te)))

let t_tv tv = O.TypeVar tv
(*let t_dog n = O.TypeName ([], [sprintf "dog_%d" n])*)
let t_unit = O.TypeConst O.TypeUnit
let t_string = O.TypeConst O.TypeString
let t_int = O.TypeConst O.TypeInt
let t_byte = O.TypeName ([],["WC";"t_byte"])
let t_float = O.TypeConst O.TypeFloat
let t_bool = O.TypeConst O.TypeBool
let t_option t = O.TypeName ([t], ["option"])
let t_list t = O.TypeName ([t], ["list"])
let t_choice = function
  | [] -> t_unit
  | tt -> O.TypeName (tt, [sprintf "%s_choice%d" tn_prefix (List.length tt)])
let t_name0 tvs name = O.TypeName (tvs, [name])
let t_name from ?(prev=false) ctxt tvs _type = function
  | "unit" -> ctxt, t_unit
  | "string" -> ctxt, t_string
  | "boolean" -> ctxt, t_bool
  | "int" | "integer" | "short" | "long" | "byte" -> ctxt, t_int
  | "double" | "float" | "decimal" -> ctxt, t_float
  | name ->
      if StringSet.mem name ctxt.tnames
      then
        let ctxt, _, typename = get_typename (from^"->t_name") ~bump:false ~prev ctxt tvs name _type in
        ctxt, O.TypeName (tvs, [typename])
      else ctxt, O.TypeName (tvs, [name])
let t_tuple typs = O.TypeTuple typs
(*let t_Time_t = O.TypeName ([],["Time";"t"])*)
let t_dateTime = O.TypeName ([],["WC";"t_dateTime"])

let tv_names tvs = List.fold_left (fun tns -> function O.TypeVar tv -> tns@[tv] | _ -> tns) [] tvs

let typemod name mino maxo _type =
  match mino, maxo with
  | (Some "0",Some "0") -> nprintf "typemod(0,0): %s -> unit\n%!" name; t_unit
  | (Some "0",Some "1")
  | (Some "0",None) -> nprintf "typemod(0,1): %s -> option\n%!" name; t_option _type
  | (Some "1",Some "1")
  | (None,None) -> nprintf "typemod(_,_): %s -> default\n%!" name; _type
  | _ -> nprintf "typemod(_): %s -> list\n%!" name; t_list _type

let ta ctxt =
  if ctxt.first
  then (ctxt.first <- false; "type")
  else "and"

let add_type ctxt (tvs,tn) v =
  if TypeMap.mem (tvs,tn) ctxt.typmap
  then (nprintf "duplicate type: %s %s\n%!" (tvstr tvs) tn; ctxt, false)
  else (nprintf "add_type: %s %s\n%!" (tvstr tvs) tn;
        { ctxt with typmap = TypeMap.add (tvs,tn) v ctxt.typmap }, true)

let stdtype ctxt oc mino maxo typo name tname =
  let typetype = typemod name mino maxo tname in
  let ctxt, iname, typename = get_typename ~bump:false "stdtyp" ctxt [] name typetype in
  nprintf "stdtyp: name=%s iname=%s typename=%s\n%!" name iname typename;
  let ctxt, _, cn = get_consname ~bump:true "stdtyp" ctxt [] name typetype in
  let ctxt, added = add_type ctxt ([],typename) (true,true,name,iname,mino,maxo,typetype) in
  nprintf "stdtyp: name=%s typename=%s tname=%s typetype=%s added=%b\n" name iname (ste tname) (ste typetype) added;
  if added then fprintf oc "%s %s =\n  %s of %s\n" (ta ctxt) typename cn (ste typetype);
  ctxt, ([], O.TypeName([],[typename]))

let mktyp ctxt oc mino maxo typo name =
  (* TODO: we should look up the value in the given namespace *)
  match Option.map (Str.split recolon) typo with
  | (Some ["dateTime"])
  | (Some [_;"dateTime"]) -> stdtype ctxt oc mino maxo typo name t_dateTime
  | (Some ["string"])
  | (Some [_;"string"]) -> stdtype ctxt oc mino maxo typo name t_string
  | (Some ["int"])
  | (Some [_;"int"])
  | (Some ["integer"])
  | (Some [_;"integer"])
  | (Some ["short"])
  | (Some [_;"short"])
  | (Some ["long"])
  | (Some [_;"long"]) -> stdtype ctxt oc mino maxo typo name t_int (* TODO: Range checks *)
  | (Some ["byte"])
  | (Some [_;"byte"]) -> stdtype ctxt oc mino maxo typo name t_byte (* TODO: Range checks *)
  | (Some ["double"])
  | (Some [_;"double"])
  | (Some ["float"])
  | (Some [_;"float"])
  | (Some ["decimal"])
  | (Some [_;"decimal"]) -> stdtype ctxt oc mino maxo typo name t_float (* TODO: NaN and INF *)
  | (Some ["boolean"])
  | (Some [_;"boolean"]) -> stdtype ctxt oc mino maxo typo name t_bool
  | (Some [tv]) when tv.[0] = '\'' ->
      let tvar = t_tv tv in
      let typetype = typemod name mino maxo tvar in
      let ctxt, iname, typename = get_typename ~bump:false "mktyp" ctxt [tvar] name typetype in
      let ctxt, added = add_type ctxt ([tv],typename) (false,true,name,iname,mino,maxo,typetype) in
      nprintf "mktyp: name=%s iname=%s typename=%s added=%b\n%!" name iname typename added;
      if added then fprintf oc "%s %s %s =\n  %s\n" (ta ctxt) tv typename (ste typetype);
      ctxt, ([tvar], O.TypeName([tvar],[typename]))
  | (Some [_;tn]) ->
      let marker_type = t_name0 [] (tn_prefix^"_"^tn) in
      let ctxt, tname = t_name "mktyp" ~prev:true ctxt [] marker_type tn in
      let typetype = typemod name mino maxo tname in
      let ctxt, iname, typename = get_typename ~bump:false "mktyp" ctxt [] name typetype in
      let new_type = t_name0 [] typename in
      let ctxt, _, cn = get_consname ~bump:true "mktyp" ctxt [] name typetype in
      let ctxt, added = add_type ctxt ([],typename) (true,true,name,iname,mino,maxo,typetype) in
      if added then fprintf oc "%s %s =\n  %s of %s\n" (ta ctxt) typename cn (ste typetype);
      nprintf "mktyp: name=%s iname=%s typename=%s\n%!" name iname typename;
      nprintf "mktyp: typetype=%s new_type=%s added=%b\n%!" (ste typetype) (ste new_type) added;
      ctxt, ([], new_type)
  | _ -> raise (Failure (sprintf "Test_wsdl.typemod: Unknown type=(%s %s %s %s)" (ot mino) (ot maxo) (ot typo) name))

let get_element_atts _name atts =
  let mino = WC.find_att ("","minOccurs") atts in
  let maxo = WC.find_att ("","maxOccurs") atts in
  let typo = WC.find_att ("","type") atts in
  (*eprintf "element: %s" _name;
  (match mino with Some mino -> eprintf " minOccurs=%s" mino | None -> ());
  (match maxo with Some maxo -> eprintf " maxOccurs=%s" maxo | None -> ());
  (match typo with Some typo -> eprintf " type=%s" typo | None -> ());
  eprintf "\n";*)
  (mino,maxo,typo)

let get_att_over attname att_opt atts =
  match WC.find_att ("",attname) atts with
  | Some att -> Some att
  | None -> att_opt

let opt_prec opt1 opt2 =
  match opt1, opt2 with
  | Some v1, Some v2 -> Some v1
  | Some v1, None -> Some v1
  | None, Some v2 -> Some v2
  | None, None -> None

let get_asc trees =
  List.fold_left 
    (fun (alls,seqs,chcs) -> function
     | (WC.E (((_,"all"),_),_)) as all -> (alls@[all],seqs,chcs)
     | (WC.E (((_,"sequence"),_),_)) as seq -> (alls,seqs@[seq],chcs)
     | (WC.E (((_,"choice"),_),_)) as chc -> (alls,seqs,chcs@[chc])
     | _ -> (alls,seqs,chcs)) ([],[],[]) trees

let treenames = List.fold_left (fun acc -> function WC.E (((_,name),_),_) -> acc@[name] | _ -> acc) []
let stns trees = String.sconcat ~left:"[" ~right:"]" "; " (treenames trees)

let rec pre_types top ctxt oc trees =
  nprintf "pre_types: trees=%s\n%!" (stns trees);
  let ctxt, gtyps = List.fold_left (fun (ctxt,acc) -> function
                                    | (WC.E (((_,n),_), _trees)) as typ ->
                                        nprintf "pre_types: %s\n%!" n;
                                        let ctxt, tps = get_elements false ctxt oc [typ] in
                                        (ctxt, acc@tps)
                                    | _ -> assert false) (ctxt,[]) trees in
  let alltvs = List.concat (List.map (fun (tvs, _) -> tvs) gtyps) in
  ctxt, alltvs, gtyps

and get_sequence top ctxt oc trees =
  nprintf "get_sequence: trees=%s\n%!" (stns trees);
  let ctxt, alltvs, gtyps = pre_types top ctxt oc trees in
  let ctxt, tt =
    match gtyps with
    | [] -> ctxt, t_unit
    | [(tvs, tn)] -> ctxt, tn
    | _ ->
        let ctxt, tts =
          List.fold_left (fun (ctxt, tts) (tvs, tn) ->
                            let ctxt, typename = ctxt, tn in
                            (ctxt, tts@[typename])) (ctxt,[]) gtyps in
        ctxt, t_tuple tts
  in
  nprintf "get_sequence: tt=%s\n%!" (ste tt);
  ctxt, [alltvs, tt]

and get_choice top ctxt oc trees =
  nprintf "get_choice: trees=%s\n%!" (stns trees);
  let ctxt, alltvs, gtyps = pre_types top ctxt oc trees in
  let cn = ctxt.cn in
  ctxt.cn <- ctxt.cn + 1;
  let ctxt, tt, chs =
    match gtyps with
    | [] -> ctxt, t_unit, []
    | [(tvs, tn)] ->
        let ctxt, typename = t_name "get_choice" ctxt tvs tn (!watchme^(ste tn)) in
        let consname = tn2cn (sprintf "Ch%d" cn) typename in
        ctxt, t_choice [typename], [(consname, Some typename)]
    | _ ->
        let ctxt, tts, chs =
          List.fold_left (fun (ctxt, tts, chs) (tvs, tn) ->
                            let ctxt, typename = t_name "get_choice" ctxt tvs tn (!watchme^(ste tn)) in
                                                                                (* ^^^^^^^--- Do something about this !!! *)
                            let consname = tn2cn (sprintf "Ch%d" cn) typename in
                            nprintf "get_choice: typename=%s consname=%s\n%!" (ste typename) consname;
                            (ctxt, tts@[typename], chs@[(consname, Some typename)])) (ctxt,[],[]) gtyps in
        ctxt, t_choice tts, chs
  in
  let name = sprintf "choice%d" cn in
  let typename = sprintf "%s_%s" tn_prefix name in
  let ct = O.TypeConstructor chs in
  let ctxt, added = add_type ctxt (tv_names alltvs,typename) (true,true,name,name,None,None,ct) in
  nprintf "get_choice: ct=%s added=%b\n%!" (ste ct) added;
  if added then fprintf oc "%s %s %s_choice%d = %s\n" (ta ctxt) (tvstr (List.map ste alltvs)) tn_prefix cn (ste ct);
  let tn = O.TypeName(alltvs,[typename]) in
  nprintf "get_choice: tn=%s\n%!" (ste tn);
  ctxt, [alltvs, O.TypeName(alltvs,[typename])]

and get_complex_types top isel ctxt oc name mino maxo trees =
  nprintf "get_complex_types: trees=%s\n%!" (stns trees);
  let ctxt, alltvs, gtyps = pre_types top ctxt oc trees in
  let ctxt, consname, tt =
    match gtyps with
    | [] ->
        (* We do actually get this: <xs:complexType name="CreateInternetGatewayType"/> *)
        let typename = t_unit in
        let typ = typemod ("0"^name) mino maxo typename in
        let ctxt, _, consname = get_consname ~bump:false "0 - get_complex_types" ctxt [] name typ in
        ctxt, consname, typ
    | [(tvs, tn)] ->
        let ctxt, typename = ctxt, tn in
        let typ = typemod ("1"^name) mino maxo typename in
        let ctxt, _, consname = get_consname ~bump:false "1 - get_complex_types" ctxt tvs name typ in
        nprintf "\nget_complex_types: consname=%s typename=%s\n%!" consname (ste typename);
        ctxt, consname, typ
    | _ ->
        let ctxt, tts =
          List.fold_left (fun (ctxt, tts) (tvs, tn) ->
                            let ctxt, typename = ctxt, tn in
                            nprintf "\nget_complex_types: typename=%s\n%!" (ste typename);
                            (ctxt, tts@[typename])) (ctxt,[]) gtyps in
        let typ = typemod ("2"^name) mino maxo (t_tuple tts) in
        let ctxt, _, consname = get_consname ~bump:false "2 - get_complex_types" ctxt alltvs name typ in
        nprintf "\n\nget_complex_types: consname=%s typ=%s\n\n%!" consname (ste typ);
        ctxt, consname, t_tuple tts
  in
  let ctxt, tname = t_name "get_complex_types" ctxt alltvs tt name in
  let ctxt = equalise_types ctxt name tt tname in
  let ctxt, iname, typename = get_typename ~bump:true "3 - get_complex_types" ctxt alltvs name tname in
  let ctxt, added = add_type ctxt (tv_names alltvs,typename) (true,isel,name,iname,mino,maxo,tt) in
  if added
  then
    (if isel
     then fprintf oc "%s %s =\n  %s of %s\n\n" (ta ctxt) (ste tname) consname (ste tt)
     else fprintf oc "%s %s =\n  %s\n\n" (ta ctxt) (ste tname) (ste tt));
  let tn = O.TypeName (alltvs,[typename]) in
  nprintf "\nget_complex_types: tname=%s tn=%s tt=%s added=%b\n%!" (ste tname) (ste tn) (ste tt) added;
  ctxt, [alltvs, tn]

and get_elcts top isel ctxt oc atts trees =
  nprintf "get_elcts: trees=%s\n%!" (stns trees);
  match WC.find_att ("","name") atts with
  | Some name ->
      let oldname = !dname in
      dname := name;
      nprintf "get_elcts: atts=%s\n%!" (WC.satts atts);
      let mino, maxo, typo = get_element_atts name atts in
      nprintf "get_elcts: name=%s mino='%s' maxo='%s'\n%!" name (os mino) (os maxo);
      (match typo with
       | Some _ ->
           let ctxt, (tvs, tn) = mktyp ctxt oc mino maxo typo name in
           nprintf "get_elcts: name=%s tn=%s\n%!" name (ste tn);
           let res = ctxt, [(tvs, tn)] in
           dname := oldname;
           res
       | None ->
           let res = get_complex_types top isel ctxt oc name mino maxo trees in
           dname := oldname;
           res)
  | None ->
      get_sequence top ctxt oc trees

and elname atts =
  match WC.find_att ("","name") atts with
  | Some name -> name
  | None -> "<no name>"

and get_element top ctxt oc = function
  | WC.E (((_,"complexType"),atts), trees) ->
      let oldname = !dname in
      dname := elname atts;
      nprintf "get_element(%s): complexType\n%!" (elname atts);
      let res = get_elcts top false ctxt oc atts trees in
      dname := oldname;
      res
  | WC.E (((_,"element"),atts), trees) ->
      let oldname = !dname in
      dname := elname atts;
      nprintf "get_element(%s): element\n%!" (elname atts);
      let res = get_elcts top true ctxt oc atts trees in
      dname := oldname;
      res
  | WC.E (((_,"all"),atts), trees) ->
      nprintf "get_element: all\n%!";
      get_elcts top false ctxt oc atts trees
  | WC.E (((_,"sequence"),atts), trees) ->
      nprintf "get_element: sequence\n%!";
      get_sequence top ctxt oc trees
  | WC.E (((_,"choice"),atts), trees) ->
      nprintf "get_element: choice\n%!";
      get_choice top ctxt oc trees
  | WC.E (((_,"any"),atts), _trees) ->
      nprintf "get_element: any\n%!";
      let mino, maxo, _typo = get_element_atts "<any>" atts in
      let tv = tvar () in
      let ctxt, (tvs, tn) = mktyp ctxt oc mino maxo (Some ("'a"^tv)) ("Any"^tv) in
      ctxt, [(tvs, tn)]
  | _ -> ctxt, []

and get_elements top ctxt oc trees =
  let els = List.filter (function
                         | WC.E (((_,("complexType"|"element"|"all"|"sequence"|"choice"|"any")),_),_) -> true
                         | _ -> false) trees in
  (*eprintf "%d elements\n%!" (List.length els);*)
  List.fold_left (fun (ctxt,acc) el ->
                    let ctxt, els = get_element top ctxt oc el in
                    (ctxt,acc@els)) (ctxt,[]) els

let get_schemas ctxt oc tree =
  let rec aux = function
    | WC.E (((_,"schema"),atts), trees) ->
        let _efd = WC.find_att ("", "elementFormDefault") atts in
        let _targns = WC.find_att ("", "targetNamespace") atts in
        (*eprintf "schema\n";*)
        get_elements true ctxt oc trees
    | _ -> ctxt, []
  in
  aux tree

(* Start of generation phases *)

(* Support code *)

let arg_of_tvn tvn = String.sub tvn 1 (String.length tvn - 1)
let arg_of_tv = function O.TypeVar tv -> String.sub tv 1 (String.length tv - 1) | _ -> "_"

let make_type_name tn = String.concat "." tn

let name_of_type_name tn = String.sub tn tnplen (String.length tn - tnplen)

let getvidx name ai =
  try
    let idx = StringMap.find name ai in
    let ai = StringMap.add name (idx+1) ai in
    ai, idx
  with Not_found ->
    StringMap.add name 1 ai, 0

let is_caml_keyword w =
  List.mem w [ "assert"; "with"; "while"; "when"; "virtual"; "val"; "type"; "try"; "true"; "to"; "then"; "struct";
               "sig"; "rec"; "private"; "or"; "open"; "of"; "object"; "new"; "mutable"; "module"; "mod"; "method";
               "match"; "lxor"; "lsr"; "lsl"; "lor"; "let"; "lazy"; "land"; "initializer"; "inherit"; "include"; "in";
               "if"; "functor"; "function"; "fun"; "for"; "false"; "external"; "exception"; "end"; "else"; "downto";
               "done"; "do"; "constraint"; "class"; "begin"; "asr"; "as"; "and"; ]

let camlvar v = if Char.is_upper v.[0] || is_caml_keyword v then "_"^v else v

let getvname pname cname ai cnt =
  let arg =
    if pname = "top" || pname = ""
    then cname
    else if Char.is_upper pname.[0] || is_caml_keyword pname then "_"^pname else pname
  in
  let ai, idx = getvidx arg ai in
  let idxstr = if cnt > 0 && idx > 0 then string_of_int idx else "" in
  dname := cname;
  (*if pname = "code" then dprintf "getvname: pname=%s cname=%s arg=%s idxstr=%s\n" pname cname arg idxstr;*)
  ai, arg^idxstr

let anyre = Str.regexp "Any[0-9]+"
let is_any name = Str.string_match anyre name 0
let choicere = Str.regexp "choice[0-9]+"
let is_choice name = Str.string_match choicere name 0

let deoptarg s = if s = "" then "" else if s.[0] = '?' || s.[0] = '!' then String.sub s 1 (String.length s - 1) else s

let is_type_const = function
  | O.TypeVar _ -> true
  | O.TypeName (_, ["int"]) -> true
  | O.TypeName ([], ["WC";"t_dateTime"]) -> true
  | O.TypeName ([], ["WC";"t_byte"]) -> true
  | O.TypeConst _ -> true
  | O.TypeName (_, [tn]) -> is_any (name_of_type_name tn) (* t_Any !!! *)
  | _ -> false

let tvre = Str.regexp "'a\\([0-9]+\\)"
let compare_tv tv1 tv2 =
  try
    let tf1 = Str.string_match tvre tv1 0 in
    let n1 = int_of_string (Str.matched_group 1 tv1) in
    let tf2 = Str.string_match tvre tv2 0 in
    let n2 = int_of_string (Str.matched_group 1 tv2) in
    (match tf1, tf2 with
     | true, true -> Pervasives.compare n1 n2
     | _, _ -> String.compare tv1 tv2)
  with Failure _ | Invalid_argument _ -> String.compare tv1 tv2

let split_idx str =
  if str = ""
  then "", ""
  else
    let start = String.length str - 1 in
    let p = ref start in
    let go = ref true in
    while !go && !p >= 0 do if Char.is_digit str.[!p] then decr p else go := false done;
    if !p >= 0 then if str.[!p] = '_' then decr p;
    let pre = String.sub str 0 (!p + 1) in
    let idx = String.sub str (!p + 1) (start - !p) in
    pre, idx

(* Make convenience functions *)

let make_type_const pname ai te =
  let cname, cnt =
    match te with
    | O.TypeConst O.TypeString -> "str",1
    | O.TypeConst O.TypeInt
    | O.TypeConst O.TypeInt64 -> "i64",1
    | O.TypeConst O.TypeFloat -> "f",1
    | O.TypeConst O.TypeBool -> "b",1
    | O.TypeConst O.TypeUnit -> "()",0
    | O.TypeName ([], ["int"]) -> "i",1
    | O.TypeName ([], ["WC";"t_dateTime"]) -> "dT",1
    | O.TypeName ([], ["WC";"t_byte"]) -> "byte",1
    | _ -> "v",1
  in
  let ai, arg = getvname pname cname ai cnt in
  [arg], ai, arg

let rec make_type pname tm (ai:int StringMap.t) = function
  | O.TypeVar tv ->
      let arg = arg_of_tvn tv in
      [arg], ai, arg
  | O.TypeName (_, ["int"]) as t ->
      make_type_const pname ai t
  | O.TypeName ([], ["WC";"t_dateTime"]) as t ->
      make_type_const pname ai t
  | O.TypeName ([], ["WC";"t_byte"]) as t ->
      make_type_const pname ai t
  | O.TypeName ([tv], ["option"]) ->
      let args, ai, tpv = make_type pname tm ai tv in
      let itc, spre, spost = if is_type_const tv then true, "", "" else false, "(Some ", ")" in
      let args =
        if List.length args = 1 && itc
        then List.map (fun s -> "?"^s) args
        else args
      in
      args, ai, sprintf "%s(%s)%s" spre tpv spost
  | O.TypeName ([tv], ["list"]) ->
      let args, ai, tpv = make_type pname tm ai tv in
      let p = sprintf "(%s)" (String.concat "," (List.map deoptarg args)) in
      let ai, arg = getvname pname "lst" ai 1 in
      ["!"^arg], ai, sprintf "(List.map (function %s -> %s) %s)" p tpv arg
  | O.TypeName (tvs, [tn]) ->
      (try
         let has_con, isel, _name, iname, _, _, te = TypeMap.find (tv_names tvs,tn) tm in
         if is_choice iname
         then [iname], ai, iname
         else
           let args, ai, tpv = make_type _name tm ai te in
           args, ai, (if has_con && isel
                      then sprintf "(%s_%s %s)" cn_prefix iname tpv
                      else sprintf "%s" tpv)
       with Not_found ->
         let args, ai, tpvs = make_types pname tm ai tvs in
         args, ai, sprintf "(%s %s)" tn (String.concat " " tpvs))
  | O.TypeName (tvs, tn) ->
      let args, ai, tpvs = make_types pname tm ai tvs in
      args, ai, sprintf "(%s %s)" (make_type_name tn) (String.concat " " tpvs)
  | (O.TypeConst cte) as te ->
      make_type_const pname ai te
  | O.TypeTuple tes ->
      (match tes with
       | [] -> [], ai, "()"
       | [te] -> make_type pname tm ai te
       | tes ->
           let args, ai, tpvs = make_types pname tm ai tes in
           args, ai, String.sconcat ~left:"(" ~right:")" ", " tpvs)
  (*| O.TypeRef of type_expr*)
  (*| O.TypeRecord of (bool (* mutable *) * string * type_expr) list*)
  (*| O.TypeConstructor of (string * type_expr option) list*)
  (*| O.TypeArrow of type_expr * type_expr*)
  (*| O.TypeLabel of bool (* optional *) * string * type_expr*)
  (*| O.TypeVerbatim of string*)
  | _ -> [], ai, "<dongle>"

and make_types pname tm ai tes =
  List.fold_left (fun (args,ai,tpvs) te ->
                    let args2, ai, tpv = make_type pname tm ai te in
                    (args@args2), ai, (tpvs@[tpv])) ([],ai,[]) tes

let make_t ctxt oc =
  let tvs, cons =
    TypeMap.fold (fun (tvs,tn) (has_con,_isel,_name,_iname,_mino,_maxo,_tt) (tvs2,cons) ->
                    if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                    then (tvs2,cons)
                    else
                      let name = String.sub tn tnplen (String.length tn - tnplen) in
                      if is_choice name
                      then (tvs2,cons)
                      else tvs@tvs2, cons@[sprintf "  | `%s_%s of %s t_%s\n" cc_prefix name (tvstr tvs) name])
      ctxt.typmap ([],[]) in
  let tvs = List.uniq ~cmp:String.compare (List.sort compare_tv tvs) in
  (*eprintf "tvs: %s\n%!" (String.sconcat ~left:"[" ~right:"]" "; " tvs);*)
  let type_t = sprintf "%s t" (tvstr tvs) in
  fprintf oc "\ntype %s t = [\n%s]\n" (tvstr tvs) (String.concat "" cons);
  { ctxt with type_t = type_t }

let get_arg param a =
  if a = "?_top"
  then false, "_top"
  else if a.[0] = '?'
  then true, a
  else if a.[0] = '!'
  then
    let a = String.sub a 1 (String.length a - 1) in
    true, if param then sprintf "?%s" a else sprintf "?(%s=[])" a
  else if a = "_top" || a = "()"
  then false, a
  else false, "~"^a

let get_args ?(param=false) = function
  | [] -> "()"
  | [tv] -> let isopt, a = get_arg param tv in if isopt then sprintf "%s ()" a else a
  | tvs ->
      let opts, _nonopts, args =
        List.fold_left (fun (opts,nonopts,aa) s ->
                          let isopt, a = get_arg param s in
                          if isopt then (opts+1,nonopts,aa@[a]) else (opts,nonopts+1,aa@[a]))
          (0,0,[]) tvs
      in
      let args = String.concat " " args in
      if opts > 0 then args^" ()" else args

let _Chre = Str.regexp "\\([ ]*Ch\\([0-9]+\\)_\\)\\(.*+\\)"
let chpre str =
  let pre, idx = split_idx str in
  if Str.string_match _Chre pre 0
  then Str.matched_group 1 pre, Str.matched_group 2 pre, Str.matched_group 3 pre, idx
  else "", "", "", ""

let make_choices ctxt oc chname = function
  | O.TypeConstructor cl ->
      List.iter (fun (name,teo) ->
                   match teo with
                   | Some te ->
                       let pre, num, n, idx = chpre name in (* <--- Won't work with type variables !!!! *)
                       let ni = n^idx in
                       let args, _, expr = make_type "top" ctxt.typmap StringMap.empty te in
                       (*eprintf "make_choices: name=%s pre='%s' n='%s' num=%s\n%!" name pre n num;*)
                       fprintf oc "let %s_%s %s = (Ch%s_%s %s)\n\n" chname ni (get_args args) num ni expr
                   | None -> eprintf "Choice without constructors\n%!"
                ) cl
  | _ -> eprintf "Choice not TypeConstructor\n%!"

let is_null mino maxo =
  match mino, maxo with
  | Some "0", Some "0" -> true
  | _, _ -> false

let make_conveniences ctxt oc =
  fprintf oc "\n(* Convenience functions *)\n\n";
  (*print_typmap ctxt;*)
  TypeMap.iter (fun (_tvs,tn) (has_con,isel,_name,iname,_mino,_maxo,tt) ->
                  if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                  then ()(*eprintf "make_conveniences: rejecting type %s\n%!" tn*)
                  else
                    (if is_choice iname
                     then make_choices ctxt oc iname tt
                     else
                       let args, _, expr = make_type "top" ctxt.typmap StringMap.empty tt in
                       (*eprintf "%s: (%s,%s)\n%!" (string_of_tn (_tvs,tn)) (os _mino) (os _maxo);*)
                       let cname = if isel then cn_prefix^"_"^iname else "" in
                       if is_null _mino _maxo
                       then
                         (fprintf oc "let make_%s %s =\n  (%s None)\n\n" iname (get_args args) cname;
                          if !collect then fprintf oc "let make_%s_t %s =\n  %s (%s None)\n\n"
                                                      iname (get_args args) ("`"^cc_prefix^"_"^iname) cname)
                       else
                         (fprintf oc "let make_%s %s =\n  (%s (%s))\n\n" iname (get_args args) cname expr;
                          if !collect then fprintf oc "let make_%s_t %s =\n  %s (%s (%s))\n\n"
                                                      iname (get_args args) ("`"^cc_prefix^"_"^iname) cname expr)))
               ctxt.typmap

(* End of convenience functions *)

let make_string_const ai cte =
  let convfn, cname, cnt =
    match cte with
    | O.TypeString -> "","str",1
    | O.TypeInt -> "string_of_int","i",1
    | O.TypeInt64 -> "Int64.to_string","i64",1
    | O.TypeFloat -> "string_of_float","f",1
    | O.TypeBool -> "string_of_bool","b",1
    | O.TypeUnit -> "","()",0
  in
  let arg = cname^(if cnt > 0 then string_of_int ai else "") in
  arg, ai+cnt, convfn

(* Make post header functions *)

type pname = { pn_idx:bool; pn_opt:bool; pn_ipath:string list; pn_path:string list; }

let string_of_pname {pn_idx; pn_opt; pn_ipath; pn_path;} =
  sprintf "(%b,%b,%s,%s)" pn_idx pn_opt (String.concat "." pn_ipath) (String.concat "." pn_path)

let pn_init = { pn_idx=false; pn_opt=false; pn_ipath=[]; pn_path=["top"]; }

let is_pntop = function { pn_path=["top"]; _; } -> true | _ -> false

let set_pn_idx pn idx = { pn with pn_idx = idx }
let set_pn_opt pn opt = { pn with pn_opt = opt }
let add_pn_path pn path = { pn with pn_path=(pn.pn_path@[path]) }
let add_pn_ipath pn path = { pn with pn_ipath=(pn.pn_ipath@[path]) }

let getpnv pn = String.capitalize (camlvar (List.last pn))

let getpn pname =
  match pname with
  | { pn_idx=true; pn_path=("top"::pn); _; } -> getpnv pn
  | { pn_idx=false; pn_path=("top"::pn); _; } -> String.concat "." (List.map String.capitalize pn)
  | { pn_path=pn; _; } -> raise (Failure (sprintf "getpn: not Top %s" (String.concat ":" pn)))
let getpns pname =
  let pn = getpn pname in
  match pname.pn_ipath with
  | [] -> false, [pn]
  | ipns -> if pn = getpnv [(List.last ipns)] then true, ipns else false, (ipns@[pn])
let getvpn { pn_path=pname; _; } = camlvar (List.last pname)

let is_set s =
  let l = String.length s in
  if l < 3
  then false
  else s.[l-3] = 'S' && s.[l-2] = 'e' && s.[l-1] = 't'

let sets = ["filterSet";"valueSet"]

let is_set_name pname = List.exists (fun n -> List.mem n pname.pn_path) sets

let list_idx i = Char.chr ((Char.code 'i')+i)

let ph_type_const pname ai te =
  (*if is_set_name pname then eprintf "pname: %s\n%!" (string_of_pname pname);*)
  let convfn, cname, typ, cnt =
    match te with
    | O.TypeConst O.TypeString -> "((","str","):string)",1
    | O.TypeConst O.TypeInt -> "string_of_int","i","",1
    | O.TypeConst O.TypeInt64 -> "Int64.to_string","i64","",1
    | O.TypeConst O.TypeFloat -> "string_of_float","f","",1
    | O.TypeConst O.TypeBool -> "string_of_bool","b","",1
    | O.TypeConst O.TypeUnit -> "","()","",0
    | O.TypeName ([], ["int"]) -> "string_of_int","i","",1
    | O.TypeName ([], ["WC";"t_dateTime"]) -> "WC.string_of_dateTime","dT","",1
    | O.TypeName ([], ["WC";"t_byte"]) -> "WC.string_of_byte","byte","",1
    | _ -> "(fun x -> x)","v","",1
  in
  let ai, arg = getvname (getvpn pname) cname ai cnt in
  if is_pntop pname
  then [], ai, "[]"
  else
    let p =
      if pname.pn_idx
      then
        if List.length pname.pn_ipath > 0
        then
          let do_last, pns = getpns pname in
          let l = List.length pns in
          let _Pns = List.map String.capitalize pns in
          let pnss = List.mapi (fun i pn ->
                                  if do_last || i < l - 1
                                  then sprintf "\"%s.\"^(string_of_int (__%c+1))" pn (list_idx i)
                                  else sprintf "\"%s\"" pn) _Pns in
          let pnv = String.concat "^\".\"^" pnss in
          sprintf "[((%s),(%s %s%s))]" pnv convfn arg typ
        else sprintf "[(\"%s.\"^(string_of_int (__i+1)),(%s %s%s))]" (getpn pname) convfn arg typ
      else sprintf "[(\"%s\",(%s %s%s))]" (getpn pname) convfn arg typ
    in
    if pname.pn_opt
    then ["?"^arg], ai, sprintf "(match %s with Some %s -> %s | None -> [])" arg arg p
    else [arg], ai, p

let rec ph_type pname tm (ai:int StringMap.t) = function
  | O.TypeVar tv ->
      let arg = arg_of_tvn tv in
      [arg], ai, arg
  | O.TypeName (_, ["int"]) as t ->
      ph_type_const pname ai t
  | O.TypeName ([], ["WC";"t_dateTime"]) as t ->
      ph_type_const pname ai t
  | O.TypeName ([], ["WC";"t_byte"]) as t ->
      ph_type_const pname ai t
  | O.TypeName ([tv], ["option"]) ->
      let args, ai, tpv = ph_type (set_pn_opt pname true) tm ai tv in
      let itc, spre, spost = if is_type_const tv then true, "", "" else false, "", "" in
      let args, tpv =
        if List.length args = 1 && itc
        then
          if is_pntop pname
          then [], "[]"
          else args, tpv
        else args, tpv
      in
      args, ai, sprintf "%s(%s)%s" spre tpv spost
  | O.TypeName ([tv], ["list"]) ->
      let args, ai, tpv = ph_type (set_pn_idx pname true) tm ai tv in
      let p = sprintf "(%s)" (String.concat "," (List.map deoptarg args)) in
      let ai, arg = getvname (getvpn pname) "lst" ai 1 in
      ["!"^arg], ai, sprintf "(List.concat (Base.List.mapi (fun __%c -> function %s -> %s) %s))"
                             (list_idx (max (List.length pname.pn_ipath - 1) 0)) p tpv arg
  | O.TypeName (tvs, [tn]) ->
      (try
         let _has_con, isel, _name, iname, _, _, te = TypeMap.find (tv_names tvs,tn) tm in
         if is_choice iname
         then [iname], ai, iname
         else
           let pname =
             if is_set _name && List.mem _name sets
             then add_pn_ipath pname (String.sub _name 0 (String.length _name - 3))
             else pname
           in
           let args, ai, tpv = ph_type (if isel then add_pn_path pname _name else pname) tm ai te in
           args, ai, (sprintf "%s" tpv)
       with Not_found ->
         let args, ai, tpvs = ph_types pname tm ai tvs in
         args, ai, sprintf "(%s %s)" tn (String.concat " " tpvs))
  | O.TypeName (tvs, tn) ->
      let args, ai, tpvs = ph_types pname tm ai tvs in
      args, ai, sprintf "(%s %s)" (make_type_name tn) (String.concat " " tpvs)
  | (O.TypeConst cte) as te ->
      ph_type_const pname ai te
  | O.TypeTuple tes ->
      (match tes with
       | [] -> [], ai, "()"
       | [te] -> ph_type pname tm ai te
       | tes ->
           let args, ai, tpvs = ph_types pname tm ai tes in
           args, ai, String.sconcat ~left:"(" ~right:")" " @ " tpvs)
  (*| O.TypeRef of type_expr*)
  (*| O.TypeRecord of (bool (* mutable *) * string * type_expr) list*)
  (*| O.TypeConstructor of (string * type_expr option) list*)
  (*| O.TypeArrow of type_expr * type_expr*)
  (*| O.TypeLabel of bool (* optional *) * string * type_expr*)
  (*| O.TypeVerbatim of string*)
  | _ -> [], ai, "<dongle>"

and ph_types pname tm ai tes =
  List.fold_left (fun (args,ai,tpvs) te ->
                    let args2, ai, tpv = ph_type pname tm ai te in
                    (args@args2), ai, (tpvs@[tpv])) ([],ai,[]) tes

let ph_choices ctxt oc chname = function
  | O.TypeConstructor cl ->
      List.iter (fun (name,teo) ->
                   match teo with
                   | Some te ->
                       let pre, num, n, idx = chpre name in (* <--- Won't work with type variables !!!! *)
                       let ni = n^idx in
                       let args, _, expr = ph_type pn_init ctxt.typmap StringMap.empty te in
                       (*eprintf "ph_choices: name=%s pre='%s' n='%s' num=%s\n%!" name pre n num;*)
                       fprintf oc "let ph_%s_%s %s = (%s)\n\n" chname ni (get_args args) (*num ni*) expr
                   | None -> eprintf "Choice without constructors\n%!"
                ) cl
  | _ -> eprintf "Choice not TypeConstructor\n%!"

let make_phs ctxt oc =
  fprintf oc "\n(* Post header functions *)\n\n";
  (*print_typmap ctxt;*)
  TypeMap.fold (fun (_tvs,tn) (has_con,isel,name,iname,mino,maxo,tt) phargmap ->
                  if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                  then phargmap(*eprintf "make_phs: rejecting type %s\n%!" tn*)
                  else
                    (if is_choice iname
                     then (ph_choices ctxt oc iname tt; phargmap)
                     else
                       let args, _, expr = ph_type pn_init ctxt.typmap StringMap.empty tt in
                       (*eprintf "%s: (%s,%s)\n%!" (string_of_tn (_tvs,tn)) (os _mino) (os _maxo);*)
                       let argstr = get_args args in
                       if is_null mino maxo
                       then fprintf oc "let ph_%s %s =\n  [(\"Action\",\"%s\")]\n\n" iname argstr name
                       else fprintf oc "let ph_%s %s =\n  [(\"Action\",\"%s\")]@(%s)\n\n" iname argstr name expr;
                       (name,(get_args ~param:true args))::phargmap))
               ctxt.typmap []

(* End of post header functions *)

(* XML output functions *)

let make_toxml_const ai cte =
  let convfn, cname, cnt =
    match cte with
    | O.TypeString -> "","str",1
    | O.TypeInt -> "string_of_int","i",1
    | O.TypeInt64 -> "Int64.to_string","i64",1
    | O.TypeFloat -> "string_of_float","f",1
    | O.TypeBool -> "string_of_bool","b",1
    | O.TypeUnit -> "","()",0
  in
  let arg = cname^(if cnt > 0 then string_of_int ai else "") in
  arg, ai+cnt, convfn

let rec arity = function
  | O.TypeVar _ -> 1
  | O.TypeName _ -> 1
  | O.TypeConst _ -> 1
  | O.TypeRef te -> arity te
  | O.TypeTuple tes -> List.length tes
  | O.TypeRecord _ -> 1
  | O.TypeConstructor _ -> 1
  | O.TypeArrow _ -> 1
  | O.TypeLabel (_,_,te) -> arity te
  | O.TypeVerbatim s -> raise (Failure (sprintf "Can't determine arity of verbatim: %s" s))

let rec tvs_of_te = function
  | O.TypeVar s -> [s]
  | O.TypeName (tes,_) -> List.fold_left (fun tvs te -> tvs@(tvs_of_te te)) [] tes
  | O.TypeConst _ -> []
  | O.TypeRef te -> tvs_of_te te
  | O.TypeTuple tes -> List.fold_left (fun tvs te -> tvs@(tvs_of_te te)) [] tes
  | O.TypeRecord trs -> List.fold_left (fun tvs (_,_,te) -> tvs@(tvs_of_te te)) [] trs
  | O.TypeConstructor tcs -> List.fold_left (fun tvs -> function (_,Some te) -> tvs@(tvs_of_te te) | _ -> tvs) [] tcs
  | O.TypeArrow (te1,te2) -> (tvs_of_te te1)@(tvs_of_te te2)
  | O.TypeLabel (_,_,te) -> tvs_of_te te
  | O.TypeVerbatim s -> raise (Failure (sprintf "Can't determine type variables of verbatim: %s" s))

let make_tvconv tv = "toxml_"^(arg_of_tvn tv)

let rec make_toxml tm ai = function
  | O.TypeVar tv ->
      eprintf "typevar: %s\n%!" tv;
      let arg = arg_of_tvn tv in
      let confn = sprintf "toxml_%s" arg in
      [], [arg], ai, sprintf "[%s %s]" confn arg
  | O.TypeName ([], ["WC";"t_dateTime"]) ->
      let arg = "dT"^(string_of_int ai) in
      [], [arg], ai+1, sprintf "[(WC.D (WC.string_of_dateTime %s))]" arg
  | O.TypeName ([], ["WC";"t_byte"]) ->
      let arg = "byte"^(string_of_int ai) in
      [], [arg], ai+1, sprintf "[(WC.D (WC.string_of_byte %s))]" arg
  | O.TypeName ([tv], ["option"]) ->
      let tvconvs, args, ai, tpv = make_toxml tm ai tv in
      tvconvs, args, ai, sprintf "(%s)" tpv
  | O.TypeName ([tv], ["list"]) ->
      let tvconvs, args, ai, tpv = make_toxml tm ai tv in
      tvconvs, args, ai, sprintf "((fun %s -> %s) %s)" (tvstr args) tpv (tvstr args)
  | O.TypeName (tvs, [tn]) ->
      (try
         let has_con, _isel, _name, iname, _, _, te = TypeMap.find (tv_names tvs,tn) tm in
         let arg = sprintf "v_%d" ai in
         let istv, tv = match te with O.TypeVar tv -> true, tv | _ -> false, "" in
         let tvs = tvs_of_te te in
         (*eprintf "tvs: %s\n%!" (String.concat "," tvs);*)
         let tvconv, tpv =
           if has_con
           then
             let tvconv = if tvs = [] then "" else make_tvconv (List.hd tvs) in
             (if tvconv <> "" then [tvconv] else []), sprintf "(toxml_%s %s %s)" iname tvconv arg
           else
             if istv
             then
               let tvconv = "toxml_"^(arg_of_tvn tv) in
               [tvconv], sprintf "(%s %s)" tvconv arg
             else [], sprintf "%s" arg
         in
         tvconv, [arg], ai+1, tpv
       with Not_found ->
         let tvconvs, args, ai, tpvs = make_toxmls tm ai tvs in
         tvconvs, args, ai, sprintf "(%s %s)" tn (String.concat " " tpvs))
  | O.TypeName (tvs, tn) ->
      let tvconvs, args, ai, tpvs = make_toxmls tm ai tvs in
      tvconvs, args, ai, sprintf "[%s (* arity:%d *) %s]" (make_type_name tn) (List.length tn) (String.concat " " tpvs)
  | O.TypeConst O.TypeUnit ->
      [], ["()"], ai, "[]"
  | O.TypeConst cte ->
      let arg, ai, convfn = make_toxml_const ai cte in
      [], [arg], ai, sprintf "[(WC.D (%s %s))]" convfn arg
  | O.TypeTuple tes ->
      (match tes with
       | [] -> [], [], ai, "()"
       | [te] -> make_toxml tm ai te
       | tes ->
           let tvconvs, args, ai, tpvs = make_toxmls tm ai tes in
           tvconvs, args, ai, sprintf "%s" (String.sconcat ~left:"(" ~right:")" " @ " tpvs))
  (*| O.TypeRef of type_expr*)
  (*| O.TypeRecord of (bool (* mutable *) * string * type_expr) list*)
  (*| O.TypeConstructor of (string * type_expr option) list*)
  (*| O.TypeArrow of type_expr * type_expr*)
  (*| O.TypeLabel of bool (* optional *) * string * type_expr*)
  (*| O.TypeVerbatim of string*)
  | _ -> [], [], ai, "<dongle>"

and make_toxmls tm ai tes =
  List.fold_left (fun (tvconvs,args,ai,tpvs) te ->
                    let tvconvs2, args2, ai, tpv = make_toxml tm ai te in
                    (tvconvs@tvconvs2), (args@args2), ai, (tpvs@[tpv])) ([],[],ai,[]) tes

let int_of_occurs = function
  | Some "unbounded" -> max_int
  | Some n -> (try int_of_string n with
               | Failure "int_of_string" ->
                   raise (Failure (sprintf "int_of_occurs: unknown occurs value %s" n)))
  | None -> -1

let toxml_choices tm ai = function
  | O.TypeConstructor cl ->
      let tvconvs, args, ai, tpvs =
        List.fold_left (fun (tvconvs,args,ai,tpvs) (name,teo) ->
                          match teo with
                          | Some te ->
                              let tvconvs2, args2, ai, tpv = make_toxml tm ai te in
                              (tvconvs@tvconvs2), (args@args2), ai, tpvs@[sprintf "%s %s -> %s" name (tvstr args2) tpv]
                          | None -> assert false)
                       ([], [], ai, []) cl in
      tvconvs, args, ai, sprintf "%s" (String.sconcat ~left:"(function\n    " ~right:")" "\n  | " tpvs)
  | _ ->
      eprintf "toxml_choices: not a TypeConstructor\n%!";
      [], [], ai, "<dongle>"

let make_to_xml ctxt oc =
  fprintf oc "\n(* XML output functions *)\n\n";
  let pes, choices =
    TypeMap.fold (fun (_tvs,tn) (has_con,isel,name,iname,mino,maxo,tt) (acc,choices) ->
                    if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                    then (acc,choices)
                    else
                      if is_choice iname
                      then
                        let tvconvs, pats, _, expr = toxml_choices ctxt.typmap 1 tt in
                        (acc,choices@[(tvconvs,name,iname,pats,expr,None,None)])
                      else
                        let tvconvs, pats, _, expr = make_toxml ctxt.typmap 1 tt in
                        ((acc@[tvconvs,name,iname,pats,expr,isel,mino,maxo]),choices)) ctxt.typmap ([],[]) in
  fprintf oc
    "\n\
let rec %s\n\n"
    (String.concat "\nand "
       (List.map
          (fun (c,n,ni,p,e,isel,mino,maxo) ->
             let cn = cn_prefix^"_"^ni in
             let cname, ls, ps, pe, le =
               if isel
               then cn, "[", (sprintf "WC.E ((WC.mkstag \"%s\"), (" n), "))", "]"
               else "", "", "", "", ""
             in
             match mino, maxo with
             | Some "0", Some "0" ->
                 sprintf "\
toxml_%s %s = function\n  \
  | _ -> []\n"
                   n (tvstr c)
             | Some "0", Some "1"
             | Some "0", None ->
                 sprintf "\
toxml_%s %s = function\n  \
  | (%s (Some %s)) ->\n    \
    %s%s%s%s%s\n  \
  | (%s None) -> []\n"
                   ni (tvstr c) cname (tvstr p) ls ps e pe le cname
             | Some "1", Some "1"
             | None, None ->
                 sprintf "\
toxml_%s %s = function\n  \
  | (%s %s) ->\n    \
    %s%s%s%s%s\n"
                   ni (tvstr c) cname (tvstr p) ls ps e pe le
             | _, _ ->
                 sprintf "\
toxml_%s %s = function\n  \
  | (%s []) -> []\n  \
  | (%s l) -> List.map (function %s -> %s%s%s) l\n"
                   ni (tvstr c) cname cname (tvstr p) ps e pe
          ) pes));
  fprintf oc "%s%s\n\n"
    (if choices <> [] then "and " else "")
    (String.concat "\nand "
       (List.map
          (fun (c,n,ni,p,e,mino,maxo) ->
             sprintf "toxml_%s %s = %s\n" n (tvstr c) e)
          choices))


(* End of XML output functions *)

(* XML input functions *)

let make_fromxml_const ai cte =
  let convfn, cname, cnt =
    match cte with
    | O.TypeString -> "","str",1
    | O.TypeInt -> "WC.fx \"int\" int_of_string","i",1
    | O.TypeInt64 -> "WC.fx \"int64\" Int64.of_string","i64",1
    | O.TypeFloat -> "WC.fx \"float\" float_of_string","f",1
    | O.TypeBool -> "WC.fx \"bool\" bool_of_string","b",1
    | O.TypeUnit -> "(fun u -> u)","()",0
  in
  let arg = cname^(if cnt > 0 then string_of_int ai else "") in
  arg, ai+cnt, convfn

let make_tvconv tv = "fromxml_"^(arg_of_tvn tv)

let rec make_fromxml tm ai = function
  | O.TypeVar tv ->
      eprintf "typevar: %s\n%!" tv;
      let arg = arg_of_tvn tv in
      let confn = sprintf "fromxml_%s" arg in
      [], [arg], ai, sprintf "(%s %s)" confn arg
  | O.TypeName ([], ["WC";"t_dateTime"]) ->
      let arg = "dT"^(string_of_int ai) in
      [], [arg], ai,
      (sprintf "(function [(WC.D %s)] -> (WC.fx \"dateTime\" WC.dateTime_of_string %s) | _ -> raise (WC.Wsdl2mlInputFailure \"Expected dateTime\"))" arg arg)
  | O.TypeName ([], ["WC";"t_byte"]) ->
      let arg = "byte"^(string_of_int ai) in
      [], [arg], ai,
      (sprintf "(function [(WC.D %s)] -> (WC.fx \"byte\" WC.byte_of_string %s) | _ -> raise (WC.Wsdl2mlInputFailure \"Expected byte\"))" arg arg)
  | O.TypeName ([tv], ["option"]) ->
      let tvconvs, args, ai, tpv = make_fromxml tm ai tv in
      tvconvs, args, ai, sprintf "%s" tpv
  | O.TypeName ([tv], ["list"]) ->
      let tvconvs, args, ai, tpv = make_fromxml tm ai tv in
      tvconvs, args, ai, sprintf "(%s)" tpv
  | O.TypeName (tvs, [tn]) ->
      (try
         let has_con, _isel, _name, iname, _, _, te = TypeMap.find (tv_names tvs,tn) tm in
         let arg = sprintf "v_%d" ai in
         let istv, tv = match te with O.TypeVar tv -> true, tv | _ -> false, "" in
         let tvs = tvs_of_te te in
         (*eprintf "tvs: %s\n%!" (String.concat "," tvs);*)
         let tvconv, tpv =
           if has_con
           then
             let tvconv = if tvs = [] then "" else make_tvconv (List.hd tvs) in
             (if tvconv <> "" then [tvconv] else []), sprintf "(fromxml_%s %s)" iname tvconv
           else
             if istv
             then
               let tvconv = "fromxml_"^(arg_of_tvn tv) in
               [tvconv], sprintf "%s" tvconv
             else [], sprintf "%s" arg
         in
         tvconv, [arg], ai+1, tpv
       with Not_found ->
         let tvconvs, args, ai, tpvs = make_fromxmls tm ai tvs in
         tvconvs, args, ai, sprintf "(%s %s)" tn (String.concat " " tpvs))
  | O.TypeName (tvs, tn) ->
      let tvconvs, args, ai, tpvs = make_fromxmls tm ai tvs in
      tvconvs, args, ai, sprintf "(%s (* arity:%d *) %s)" (make_type_name tn) (List.length tn) (String.concat " " tpvs)
  | O.TypeConst O.TypeString ->
      let arg, ai, convfn = make_fromxml_const ai O.TypeString in
      [], [arg], ai, (sprintf "(function [(WC.D %s)] -> %s | [] -> \"\" | _ -> raise (WC.Wsdl2mlInputFailure \"Expected const\"))"
                              arg arg)
  | O.TypeConst O.TypeUnit ->
      [], ["()"], ai, "(function [] -> () | _ -> raise (WC.Wsdl2mlInputFailure \"Expected unit\"))"
  | O.TypeConst cte ->
      let arg, ai, convfn = make_fromxml_const ai cte in
      [], [arg], ai, (sprintf "(function [(WC.D %s)] -> (%s %s) | _ -> raise (WC.Wsdl2mlInputFailure \"Expected const\"))"
                        arg convfn arg)
  | O.TypeTuple tes ->
      (match tes with
       | [] -> [], [], ai, "(function [] -> () | _ -> raise (WC.Wsdl2mlInputFailure \"Expected empty tuple\"))"
       | [te] -> make_fromxml tm ai te
       | tes ->
           let tvconvs, args, ai, tpvs = make_fromxmls tm ai tes in
           let _, targs = List.fold_left (fun (i,targs) _tpv -> (i+1,(targs@[sprintf "v_%d" i]))) (1,[]) tpvs in
           let tpvs = List.map2 (fun targ tpv -> sprintf "(%s sts)" tpv) targs tpvs in
           let f = sprintf "(function sts -> %s)" (String.sconcat ~left:"(" ~right:")" ", " tpvs) in
           (*eprintf "tvconvs: %s\n" (String.concat ", " tvconvs);
           eprintf "tpvs: %s\n" (String.concat ", " tpvs);
           eprintf "f: %s\n" f;*)
           tvconvs, args, ai, f)
  (*| O.TypeRef of type_expr*)
  (*| O.TypeRecord of (bool (* mutable *) * string * type_expr) list*)
  (*| O.TypeConstructor of (string * type_expr option) list*)
  (*| O.TypeArrow of type_expr * type_expr*)
  (*| O.TypeLabel of bool (* optional *) * string * type_expr*)
  (*| O.TypeVerbatim of string*)
  | _ -> [], [], ai, "<dongle>"

and make_fromxmls tm ai tes =
  List.fold_left (fun (tvconvs,args,ai,tpvs) te ->
                    let tvconvs2, args2, ai, tpv = make_fromxml tm ai te in
                    (tvconvs@tvconvs2), (args@args2), ai, (tpvs@[tpv])) ([],[],ai,[]) tes

let fromxml_choices tm ai = function
  | O.TypeConstructor cl ->
      let tvconvs, args, ai, tpvs, ns =
        List.fold_left
          (fun (tvconvs,args,ai,tpvs,ns) (name,teo) ->
             match teo with
             | Some te ->
                 let tvconvs2, args2, ai, tpv = make_fromxml tm ai te in
                 let pre, num, n, idx = chpre name in (* <--- Won't work with type variables !!!! *)
                 let ni = n^idx in
                 (*eprintf "fromxml_choices: name=%s n=%s ni=%s\n%!" name n ni;*)
                 ((tvconvs@tvconvs2), (args@args2), ai,
                  (tpvs@[sprintf "[WC.E (((_,\"%s\"),_),_)] as sts -> (Ch%s_%s (%s %s sts))" n num ni tpv (tvstr tvconvs2)]),
                  (ns@[n]))
             | None ->
                 (eprintf "fromxml_choices: No constructor\n%!";
                  (tvconvs,args,ai,tpvs,ns))
          ) ([], [], ai, [], []) cl in
      let fail = sprintf "_ -> raise (WC.Wsdl2mlInputFailure \"Expected (%s)\")" (String.concat ", " ns) in
      tvconvs, args, ai, (sprintf "%s" (String.sconcat ~left:"(function\n    " ~right:")" "\n  | " (tpvs@[fail]))), ns
  | _ ->
      eprintf "fromxml_choices: not a TypeConstructor\n%!";
      [], [], ai, "<dongle>", []

let make_from_xml ctxt oc =
  fprintf oc "\n(* XML input functions *)\n\n";
  let pes, choices =
    TypeMap.fold (fun (_tvs,tn) (has_con,isel,name,iname,mino,maxo,tt) (acc,choices) ->
                    if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                    then (acc,choices)
                    else
                      if is_choice iname
                      then
                        let tvconvs, pats, _, expr, ns = fromxml_choices ctxt.typmap 1 tt in
                        (acc,(choices@[(tvconvs,name,iname,pats,expr,ns)]))
                      else
                        let tvconvs, pats, _, expr = make_fromxml ctxt.typmap 1 tt in
                        ((acc@[tvconvs,name,iname,pats,expr,isel,mino,maxo]),choices)) ctxt.typmap ([],[])
  in
  fprintf oc
    "\
let rec %s\n\n"
    (String.concat "and "
       (List.map
          (fun (c,n,ni,p,e,isel,mino,maxo) ->
             let cn = cn_prefix^"_"^ni in
             let cname, ls, ps, pe, le, find, fail =
               if isel
               then (cn, "[", (sprintf "WC.E (((_,\"%s\"),_), (" n), "))", "]",
                     sprintf "(WC.find_name \"%s\" sts)" n,
                     sprintf "   | _ -> raise (WC.Wsdl2mlInputFailure \"Expected %s\")\n" ni)
               else "", "", "", "", "", "sts", ""
             in
             match mino, maxo with
             | Some "0", Some "0" ->
                 let fail = sprintf "raise (WC.Wsdl2mlInputFailure \"%s has minOccurs,maxOccurs = 0,0\")" n in
                 sprintf "fromxml_%s %s = function (*0,0*)\n   | [] -> None\n%s\n\n" ni (tvstr c) fail
             | Some "0", Some "1"
             | Some "0", None ->
                 let pat0 = sprintf "[]" in
                 let exp0 = sprintf "(%s None)" cname in
                 let pat1 = sprintf "%s%ssts%s%s" ls ps pe le in
                 let exp1 = sprintf "(%s (Some (%s sts)))" cname e in
                 sprintf "fromxml_%s %s sts = (function (*%s,%s*)\n   | %s -> %s\n   | %s -> %s\n%s) %s\n\n"
                         ni (tvstr c) (os mino) (os maxo) pat0 exp0 pat1 exp1 fail find
             | Some "1", Some "1"
             | None, None ->
                 let pat = sprintf "%s%ssts%s%s" ls ps pe le in
                 let exp = sprintf "(%s (%s sts))" cname e in
                 if isel
                 then sprintf "fromxml_%s %s sts = (function (*%s,%s*)\n   | %s -> %s\n%s) %s\n\n"
                              ni (tvstr c) (os mino) (os maxo) pat exp fail find
                 else sprintf "fromxml_%s %s sts = %s\n\n" ni (tvstr c) exp
             | _, _ ->
                 let pat = sprintf "%ssts%s" ps pe in
                 let exp = sprintf "(%s sts)" e in
                 sprintf "fromxml_%s %s sts = (*%s,%s*)\n   %s (List.map (function\n   | %s -> %s\n%s) sts)\n\n"
                         ni (tvstr c) (os mino) (os maxo) cname pat exp fail)
          pes));
  fprintf oc "%s %s\n\n"
    (if choices <> [] then "and " else "")
    (String.concat "and "
       (List.map
          (fun (c,n,ni,p,e,ns) ->
             sprintf "fromxml_%s %s sts = %s\n  (WC.find_names [%s] sts)\n"
                     n (tvstr c) e (String.concat "; " (List.map (fun s -> "\""^s^"\"") ns)))
          choices))

(* End of XML input functions *)

let zip2 l1 l2 =
  let rec aux = function
    | ([],[]) -> []
    | (h1::t1,h2::t2) -> (h1,h2)::aux (t1,t2)
    | _ -> raise (Failure "zip2: unequal lists")
  in
  aux (l1,l2)

let cmp_fst compare (s1,_) (s2,_) = compare s1 s2
let compare_tvtoxml tv1 tv2 = cmp_fst compare_tv tv1 tv2

let make_toxml_t ctxt oc =
  let tox_of_tv tv = sprintf "toxml_%s" (arg_of_tvn tv) in
  let tvs, cons =
    TypeMap.fold (fun (tvs,tn) (has_con,_isel,_name,_iname,_mino,_maxo,_tt) (tvs2,cons) ->
                    if not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_'
                    then (tvs2,cons)
                    else
                      let name = String.sub tn tnplen (String.length tn - tnplen) in
                      if is_choice name
                      then (tvs2,cons)
                      else
                        let toxmltvs = List.map tox_of_tv tvs in
                        tvs@tvs2, cons@[sprintf "  | (`%s_%s v) -> toxml_%s %s v\n" cc_prefix name name (tvstr toxmltvs)])
      ctxt.typmap ([],[]) in
  let tvs = List.uniq ~cmp:String.compare (List.sort compare_tv tvs) in
  let toxmls = List.map tox_of_tv tvs in
  fprintf oc "\nlet toxml_t %s = function\n%s\n" (tvstr toxmls) (String.concat "" cons)

let make_fromxml_t ctxt oc =
  let fromx_of_tv tv = sprintf "fromxml_%s" (arg_of_tvn tv) in
  let typs =
    TypeMap.fold (fun (tvs,tn) (has_con,_isel,name,iname,_mino,_maxo,_tt) typs ->
                    let rec aux = function
                      | [] -> [(name,[(tvs,tn,has_con,_isel,name,iname,_mino,_maxo,_tt)])]
                      | (n,l)::rest ->
                          if n = name
                          then ((name,((tvs,tn,has_con,_isel,name,iname,_mino,_maxo,_tt)::l))::rest)
                          else (n,l)::(aux rest)
                    in
                    aux typs) ctxt.typmap []
  in
  let tvs, cons =
    List.fold_left
      (fun (tvs2,cons) (n,l) ->
         if is_choice n
         then (tvs2,cons)
         else
           let (_,ll) =
             List.partition
               (fun (tvs,tn,has_con,_isel,name,iname,_mino,_maxo,_tt) ->
                  not has_con || String.sub tn 0 (tnplen-1) <> tn_prefix || tn.[tnplen-1] <> '_') l
           in
           match ll with
           | [] -> (tvs2,cons)
           | [(tvs,tn,has_con,_isel,name,iname,_mino,_maxo,_tt)] ->
               let fromxmltvs = List.map fromx_of_tv tvs in
               tvs@tvs2, cons@[sprintf "  | [WC.E (((_,\"%s\"),_),_)] -> (`%s_%s (fromxml_%s %s t))\n"
                                       name cc_prefix iname iname (tvstr fromxmltvs)]
           | _ ->
               let tvs3, cons3 =
                 List.fold_left
                   (fun (tvs3,cons3) (tvs,tn,has_con,_isel,name,iname,_mino,_maxo,_tt) ->
                      let fromxmltvs = List.map fromx_of_tv tvs in
                      tvs@tvs3, cons3@[
                        sprintf "try (`%s_%s (fromxml_%s %s t)) with WC.Wsdl2mlInputFailure _ ->\n     "
                                cc_prefix iname iname (tvstr fromxmltvs)])
                   ([],[]) ll
               in
               let fail = sprintf "raise (WC.Wsdl2mlInputFailure \"fromxml_t: Expected %s\"))\n" n in
               tvs3@tvs2, cons@[
                 sprintf "  | [WC.E (((_,\"%s\"),_),_)] ->\n    (%s%s" n (String.concat "" cons3) fail
               ])
      ([],[]) typs
  in
  let tvs = List.uniq ~cmp:String.compare (List.sort compare_tv tvs) in
  let fromxmls = List.map fromx_of_tv tvs in
  fprintf oc "\nlet fromxml_t %s t = match t with\n%s" (tvstr fromxmls) (String.concat "" cons);
  fprintf oc "  | [WC.E (((_,s),_),_)] -> raise (WC.Wsdl2mlInputFailure (\"fromxml_t: Unknown constructor \"^s))\n";
  fprintf oc "  | _ -> raise (WC.Wsdl2mlInputFailure \"fromxml_t: Bad XML input\")\n\n"

let generate_debug_code oc =
  fprintf oc "\
let test (v,toxml,fromxml,name) =\n  \
  let vxml = toxml v in\n  \
  Printf.printf \"%%s XML = %%s\\n\" name (WC.string_of_tree (None,List.hd vxml));\n  \
  let v_ = fromxml vxml in\n  \
  Printf.printf \"%%s_good=%%b\\n\" name (v=v_)\n\
;;\n\
let toxml_anys = (WC.toxml_int,WC.toxml_bool,WC.toxml_int,WC.toxml_int,WC.toxml_int,WC.toxml_int,\n                  \
                  WC.toxml_int,WC.toxml_int,WC.toxml_int,WC.toxml_int,WC.toxml_int);;\n\
let fromxml_anys = (WC.fromxml_int,WC.fromxml_bool,WC.fromxml_int,WC.fromxml_int,WC.fromxml_int,WC.fromxml_int,\n                    \
                    WC.fromxml_int,WC.fromxml_int,WC.fromxml_int,WC.fromxml_int,WC.fromxml_int);;\n\
test ((make_Profile ~str:\"abc\" ()),toxml_Profile,fromxml_Profile,\"Profile\");;\n\
test ((make_BeginSession ~_Profile:\"abc\" ()),toxml_BeginSession,fromxml_BeginSession,\"BeginSession\");;\n\
test ((make_SubmitXml ~_Profile:\"abc\" ~a2:true ()),\n      \
      (toxml_SubmitXml (WC.toxml_int,WC.toxml_bool)),\n      \
      (fromxml_SubmitXml (WC.fromxml_int,WC.fromxml_bool)),\n      \
      \"SubmitXml\");;\n\
test ((make_MultiSubmitXml ~_Profile:\"abc\" ~a4:true ()),\n      \
      (toxml_MultiSubmitXml WC.toxml_bool),\n      \
      (fromxml_MultiSubmitXml WC.fromxml_bool),\n      \
      \"MultiSubmitXml\");;\n\n";
  if !collect
  then fprintf oc "\
let sxt = make_SubmitXml_t ~_Profile:\"abc\" ~a1:123 ~a2:true ();;\n\
test(sxt,(toxml_t toxml_anys),(fromxml_t fromxml_anys),\"SubmitXml_t\");;\n\
let msxt = make_MultiSubmitXml_t ~_Profile:\"abc\" ~a4:123 ();;\n\
test(msxt,(toxml_t toxml_anys),(fromxml_t fromxml_anys),\"MultiSubmitXml_t\");;\n\n"

let extfile filename suffix ext =
  (Tools.add_suffix (if Filename.check_suffix filename ".wsdl"
                     then Filename.chop_suffix filename ".wsdl"
                     else if Filename.check_suffix filename ".xsd"
                     then Filename.chop_suffix filename ".xsd"
                     else filename) suffix)^ext

let files = ref ([]:string list)
let it_ref = ref Idx

let rec get_names trees =
  let aux acc = function
    | WC.E (((_,("complexType"|"element")),atts),trees) ->
        (match WC.find_att ("","name") atts with
         | Some name -> name::acc
         | None -> acc)
    | _ -> acc
  in
  List.fold_left (fun acc tree -> WC.fold_tree aux acc tree) [] trees

let rec get_definitions ctxt oc trees =
  let els = List.filter (function | WC.E (((_,"definitions"),_),_) -> true | _ -> false) trees in
  dprintf "%d definitions\n%!" (List.length els);
  List.fold_left (fun (ctxt,acc) el -> let ctxt, els = get_definition ctxt oc el in (ctxt,acc@els)) (ctxt,[]) els

and get_definition ctxt oc = function
  | WC.E (((_,"definitions"),atts),trees) ->
      let _msgs = WC.find_name "message" trees in
      let _portTypes = WC.find_name "portType" trees in
      let _bindings = WC.find_name "binding" trees in
      let _services = WC.find_name "service" trees in
      dprintf "%d messages\n%d portTypes\n%d bindings\n%d services\n%!"
        (List.length _msgs) (List.length _portTypes) (List.length _bindings) (List.length _services);
      List.fold_left (fun (ctxt,acc) _service ->
                        let ctxt, a = get_service ctxt oc (_msgs,_portTypes,_bindings) _service in
                        (ctxt,acc@a)) (ctxt,[]) _services
  | WC.E (((_,n),atts),trees) -> eprintf "Not definitions: %s\n%!" n; ctxt, []
  | _ -> assert false

and get_service ctxt oc mpb = function
  | WC.E (((_,"service"),atts),trees) ->
      (match WC.find_att ("","name") atts with
       | Some name ->
           dprintf "service: name=%s\n%!" name;
           fprintf oc "(* Associated service: %s *)\n" name;
           ctxt, [(name,atts,trees,mpb)]
       | None -> ctxt, [("noname",atts,trees,mpb)])
  | WC.E (((_,n),_),_) -> eprintf "Not service: %s\n%!" n; ctxt, []
  | _ -> assert false

let find_elname elname name =
  List.find_opt (function
                 | (WC.E (((_,ename),atts),_)) when ename = elname ->
                     (match WC.find_att ("","name") atts with
                      | Some _name -> name = _name
                      | None -> false)
                 | _ -> false)

let find_message name (m,_,_) = find_elname "message" name m
let find_portType name (_,p,_) = find_elname "portType" name p
let find_binding name (_,_,b) = find_elname "binding" name b

let find_op_pt name = function
  | WC.E (((_,"portType"),atts),trees) -> find_elname "operation" name trees
  | WC.E (((_,n),_),_) -> eprintf "Not portType: %s\n%!" n; None
  | _ -> assert false

let find_el_att elname attname trees =
  match WC.find_name elname trees with
  | [WC.E (((_,ename),atts),trees)] when ename = elname ->
      WC.find_att ("",attname) atts
  | _ -> None

let find_address = find_el_att "address" "location"

let find_io sc = function
  | WC.E (((_,"operation"),atts),trees) ->
      (match WC.find_name "input" trees, WC.find_name "output" trees with
       | (([_] as input), ([_] as output)) ->
           (find_el_att "input" "message" input, find_el_att "output" "message" output)
       | _ -> None, None)
  | WC.E (((_,n),_),_) -> eprintf "Not operation: %s\n%!" n; None, None
  | _ -> assert false

let find_element (typmap,phargassoc) name =
  List.find_opt (fun ((_,_),(_,_,_name,_,_,_,_)) -> _name = name) typmap,
  List.assoc_opt name phargassoc

let get_msg_el ctxt sc name =
  match Str.split recolon name with
  | ["tns";elname]
  | [elname] ->
      let t_opt, pha_opt = find_element ctxt elname in
      (match t_opt, pha_opt with
       | (Some ((_tvs,_),(_has_con,_isel,_name,_iname,_,_,_te)), Some phargs) as type_opt ->
           dprintf "type:%s %s\n%!" _name phargs;
           fprintf sc "(* Type: %s %s *)\n\n" _name phargs;
           type_opt
       | _ ->
           eprintf "missing type %s\n%!" elname;
           fprintf sc "(* Missing type %s *)\n" elname;
           None, None)
  | _ ->
      fprintf sc "(* Element has weird name %s *)\n" name; None, None

let get_part ctxt sc = function
  | WC.E (((_,"part"),atts),trees) ->
      (match WC.find_att ("","name") atts with
       | Some partname ->
           dprintf "part: %s\n%!" partname;
           fprintf sc "(* Part: %s *)\n" partname;
           (match WC.find_att ("","element") atts with
            | Some elementname ->
                get_msg_el ctxt sc elementname
            | None ->
                fprintf sc "(* Part has no element *)\n"; None, None)
       | None ->
           fprintf sc "(* Part has no name *)\n"; None, None)
  | WC.E (((_,n),_),_) -> eprintf "Not part: %s\n%!" n; None, None
  | _ -> assert false

let get_parts ctxt sc trees = List.map (get_part ctxt sc) (WC.find_name "part" trees)

let make_message name ctxt sc mpb =
  match Str.split recolon name with
  | ["tns";msgname]
  | [msgname] ->
      (match find_message msgname mpb with
       | Some msg ->
           (match msg with
            | WC.E (((_,"message"),atts),trees) ->
                (match WC.find_att ("","name") atts with
                 | Some msgname ->
                     dprintf "message: %s\n%!" msgname;
                     fprintf sc "(* Message: %s *)\n" msgname;
                     get_parts ctxt sc trees
                 | None ->
                     fprintf sc "(* Message has no name *)\n"; [])
            | WC.E (((_,n),_),_) -> eprintf "Not message: %s\n%!" n; []
            | _ -> assert false)
       | None ->
           eprintf "missing message %s\n%!" msgname;
           fprintf sc "(* Missing message %s *)\n" msgname;
           [])
  | _ ->
      fprintf sc "(* Message has weird name %s *)\n" name; []

let make_aws_request sc opname = function
  | [], _
  | _, [] ->
      eprintf "make_aws_request: missing messages\n%!"
  | [Some ((_,_),(_,_,iname,_,_,_,_)), Some iparams], [Some ((_,_),(_,_,oname,_,_,_,_)), _] ->
      dprintf "make_aws_request: iname=%s oname=%s iparams=%s\n%!" iname oname iparams;
      fprintf sc
"let _%s aws %s ?expires cont err_cont =\n  \
  let params = Ec2types.ph_%s %s in\n  \
  AmazonEC2.request_aws aws \"%s\" Ec2types.fromxml_%s params ?expires cont err_cont\n\n"
        opname iparams iname iparams oname oname
  | _ -> eprintf "make_aws_request: Can only handle single parts\n%!"

let make_operation ctxt sc mpb typename portType = function
  | WC.E (((_,"operation"),atts),trees) ->
      (match WC.find_att ("","name") atts with
       | Some opname ->
           dprintf "operation: %s\n%!" opname;
           fprintf sc "(* Operation: %s *)\n\n" opname;
           (match find_op_pt opname portType with
            | Some ptop ->
                (match find_io sc ptop with
                 | (Some input, Some output) ->
                     dprintf "input message: %s\noutput message: %s\n%!" input output;
                     make_aws_request sc opname (make_message input ctxt sc mpb, make_message output ctxt sc mpb)
                 | _ ->
                     eprintf "operation %s missing inputs/outputs\n%!" opname;
                     fprintf sc "(* Operation %s missing inputs/outputs *)\n" opname)
            | None ->
                eprintf "cannot find operation %s\n%!" opname;
                fprintf sc "(* Type %s missing operation %s *)\n" typename opname)
       | None ->
           fprintf sc "(* Operation has no name *)\n")
  | WC.E (((_,n),_),_) -> eprintf "Not operation: %s\n%!" n; ()
  | _ -> assert false

let make_bindings ctxt sc mpb bindname = function
  | WC.E (((_,"binding"),atts),trees) ->
      (match WC.find_att ("","type") atts with
       | Some _type ->
           (match Str.split recolon _type with
            | ["tns";typename]
            | [typename] ->
                dprintf "type: %s\n%!" typename;
                fprintf sc "(* Type: %s *)\n" typename;
                (match find_portType typename mpb with
                 | Some portType ->
                     let operations = WC.find_name "operation" trees in
                     dprintf "%d operations\n%!" (List.length operations);
                     List.iter (make_operation ctxt sc mpb typename portType) (List.rev operations)
                 | None ->
                     eprintf "cannot find portType %s\n%!" typename;
                     fprintf sc "(* Binding %s missing portType %s *)\n" bindname typename)
            | _ ->
                fprintf sc "(* Binding %s has weird type %s *)\n" bindname _type)
       | None ->
           fprintf sc "(* Binding %s has no type *)\n" bindname)
  | WC.E (((_,n),_),_) -> eprintf "Not binding: %s\n%!" n; ()
  | _ -> assert false

let get_port ctxt sc mpb = function
  | WC.E (((_,"port"),atts),trees) ->
      (match WC.find_att ("","name") atts with
       | Some portname ->
           let address_opt = find_address trees in
           dprintf "port: name=%s address=%s\n%!" portname (os address_opt);
           fprintf sc "(* Port: %s Address:%s *)\n" portname (os address_opt);
           if Option.is_some address_opt then fprintf sc "let port_%s = \"%s\"\n\n" portname (Option.get address_opt);
           (match WC.find_att ("","binding") atts with
            | Some bind ->
                (match Str.split recolon bind with
                 | ["tns";bindname]
                 | [bindname] ->
                     dprintf "binding: %s\n%!" bindname;
                     fprintf sc "(* Binding: %s *)\n" bindname;
                     (match find_binding bindname mpb with
                      | Some binding ->
                          make_bindings ctxt sc mpb bindname binding
                      | None ->
                          eprintf "cannot find binding %s\n%!" bindname;
                          fprintf sc "(* Port %s missing binding %s *)\n" portname bindname)
                 | _ ->
                     fprintf sc "(* Port %s has weird binding %s *)\n" portname bind)
            | None ->
                fprintf sc "(* Port %s has no binding *)\n" portname)
       | None ->
           fprintf sc "(* Port has no name *)\n")
  | WC.E (((_,n),_),_) -> eprintf "Not port: %s\n%!" n; ()
  | _ -> assert false

let make_service ctxt phargmap filename (name,atts,trees,mpb) =
  let ctxt = (TypeMap.to_list ctxt.typmap, phargmap) in
  let sc = open_out  (extfile filename ("service"^name) ".ml") in
  let tim = Time.localtime (Time.now ()) in
  fprintf sc "(* Translated from %s\n * Date: %s %s\n *)\n\n(* Service %s *)\n\n"
             filename (Date.date2 tim) (Date.time tim) name;
  let _ports = WC.find_name "port" trees in
  dprintf "%d ports\n%!" (List.length _ports);
  List.iter (get_port ctxt sc mpb) _ports;
  close_out sc(*;
  let l = List.map (function ((_tvs,_tn),(_has_con,_isel,_name,_iname,_mino,_maxo,_te)) -> _name) ctxt in
  dprintf "ctxt: [%s]\n%!" (String.concat "; " l)*)

let open_file_hdr filename suffix ext modules desc =
  let file = extfile filename suffix ext in
  if !verbose then printf "Opened file: %s\n" file;
  let oc = open_out file in
  let tim = Time.localtime (Time.now ()) in
  fprintf oc "(* Translated from %s\n * Date: %s %s\n *)\n\n" filename (Date.date2 tim) (Date.time tim);
  if modules <> "" then fprintf oc "%s\n" modules;
  fprintf oc "(* %s *)\n\n" desc;
  oc

let _WC = "module WC = Wsdl2mlCommon\n"

let translate_file debug filename =
  let tree = ("tns", WC.get_tree_filename filename) in
  if !verbose then printf "Read file: %s\n" filename;
  let trees = (*resolve_imports tree*) [tree] in (* This doesn't really work for the moment (Http_client) *)
  let _types = WC.fold_trees find_types_ff [] trees in
  if _types = [] then failwith (sprintf "wsdl2ml: No types defined in %s%!" filename);
  let names = get_names _types in
  let tnames = StringSet.add_list names StringSet.empty in
  (*eprintf "names=[%s]\n%!" (String.concat ", " names);*)
  (*let oc = open_out (extfile filename "types" ".ml") in*)
  let oc = open_file_hdr filename "types" ".ml" _WC "Basic types" in
  (*let tc = if !mlidl then open_out (extfile filename "types" ".mlidl") else oc in*)
  let tc = if !mlidl then open_file_hdr filename "types" ".mlidl" "" "IDL file" else oc in
  let ctxt = { first = true;
               cn = 1;
               tnames = tnames;
               ctmap = StringMap.empty;
               typmap = TypeMap.empty;
               it = (!it_ref);
               ind = 0;
               type_t = "<missing>";
             } in
  let ctxt = List.fold_left (fun ctxt _type -> let ctxt, _ = get_schemas ctxt tc _type in ctxt) ctxt _types in
  let ctxt = if !collect then make_t ctxt oc else ctxt in
  if !mlidl then close_out tc;
  if !verbose then printf "Converted basic types.\n";
  make_conveniences ctxt oc;
  if !verbose then printf "Generated convenience functions.\n";
  let phargmap =
    if !post_headers
    then (let phargmap = make_phs ctxt oc in
          if !verbose then printf "Generated post header functions.\n";
          phargmap)
    else []
  in
  make_to_xml ctxt oc;
  if !collect then make_toxml_t ctxt oc;
  if !verbose then printf "Generated XML output functions.\n";
  make_from_xml ctxt oc;
  if !collect then make_fromxml_t ctxt oc;
  if !verbose then printf "Generated XML input functions.\n";
  if debug then generate_debug_code oc;
  let _definitions = WC.find_trees (WC.is_tree_name "definitions") trees in
  let ctxt, svcs = get_definitions ctxt oc _definitions in
  List.iter (make_service ctxt phargmap filename) svcs;
  close_out oc

let usage = Printf.sprintf "%s: code generation for WSDL stubs\nUsage: %s [options] <wsdl file>\n" Sys.argv.(0) Sys.argv.(0)

let _ =
  Arg.parse
    [("--debug-name", (Arg.String (fun s -> myname := s)), "<string>\tPrint debug info for named type.");
     ("--collect", (Arg.Bool (fun b -> collect := b)), (sprintf "<bool>\tAdd collect type (default: %b)." !collect));
     ("--post-headers", (Arg.Bool (fun b -> post_headers := b)), (sprintf "<bool>\tAdd POST header functions (default: %b)."
                                                                          !post_headers));
     ("--mlidl", (Arg.Bool (fun b -> mlidl := b)), (sprintf "<bool>\tOutput types as MLIDL file (default: %b)." !mlidl));
     ("-g", (Arg.Unit (fun () -> debug := true)), "Debug mode.");
     ("-v", (Arg.Unit (fun () -> verbose := true)), "Verbose mode.");
    ]
    (fun str -> files := (!files)@[str])
    (usage^"Options:");
  if !files = []
  then printf "%s\n" usage
  else List.iter (translate_file false) (!files)
