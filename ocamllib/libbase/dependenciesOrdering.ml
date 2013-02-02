(*
    Copyright Â© 2011 MLstate

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

module List = BaseList
let (|>) = InfixOperator.(|>)

(*#load "/shared/base.cmo"*)


let mapStringSet f set = StringSet.fold (fun v acc -> StringSet.add (f v) acc) set StringSet.empty ;;

let toString = Printf.sprintf ;;


(* DOC *)
type node = string
type nodeSet = StringSet.t
type nodeMap = nodeSet StringMap.t

let rec stringset_of_list l =
  List.fold_left (fun acc v-> StringSet.add v acc) StringSet.empty l

let rec stringmap_of_list l =
  List.fold_left (fun acc (key,v)-> StringMap.add key v acc) StringMap.empty l

let rec fix_point compare show f x =
  (*Printf.printf "------------------------\n";*)
  show x;
  let x' = f x in if (compare x x')=0 then x' else fix_point compare show f x'
;;



(* EXPANSION DE DEPENDANCES *)
type deps_dones_nexts = nodeSet * nodeSet * nodeSet

let init1_ddn node deps = 
  let dones = StringSet.singleton node in
  (deps, StringSet.singleton node, (StringSet.diff deps dones))

let init_ddns list = 
  (
    list 
  |> List.map (fun (key,deps)->  key, (init1_ddn key ( stringset_of_list deps)))
  |> stringmap_of_list
  )


let init_set_ddns list = 
  (
    list 
  |> List.map (fun (key,deps)->  key, (init1_ddn key (deps)))
  |> stringmap_of_list
  )



let get_deps ddn node = let (deps,_,_)=StringMap.find node ddn in deps

let show ddn = 
  StringMap.iter ( fun key (_deps,dones,nexts) ->
		     Printf.printf " %s : " key;
		     StringSet.iter (fun v->  Printf.printf " %s " v ) (get_deps ddn key);
		     Printf.printf " \t\t\t\t\t | ";
		     StringSet.iter (fun v->  Printf.printf " %s " v ) dones;
		     Printf.printf "  | ";
		     StringSet.iter (fun v->  Printf.printf " %s " v ) nexts;
		     Printf.printf "\n\n";
		 ) ddn ;;

let compare_ddn (a,b,_c) (a',b',_c')  =
  (StringSet.compare a a')*100 +  (StringSet.compare b b')*10 +  (StringSet.compare b b')*1

let compare_map_ddn m m' = StringMap.compare compare_ddn m m'
;;

(* les dépendances de mes dépendances sont mes dépendances !!! *)
(* les dépendances à traiter sont dans nexts *)
(* on evite de resuivre les dépendances déjà traités (dones) *)
let expand_1_ddn  ddn (deps,dones,nexts) =
  let ddeps = StringSet.fold (fun  v acc  ->  StringSet.union (get_deps ddn v) acc)  nexts StringSet.empty in
    (*Printf.printf " [ "; StringSet.iter (fun v->  Printf.printf " %s " v ) nexts; Printf.printf "  -> "; StringSet.iter (fun v->  Printf.printf " %s " v ) ddeps; Printf.printf " ]\n ";*)
  let dones = StringSet.union dones nexts in
  let nexts =  StringSet.diff ddeps dones in
  let deps = StringSet.union deps nexts in 
    (deps,dones,nexts)


let expand_map_ddn m = StringMap.map (expand_1_ddn m) m ;;

let complete_expansion deps_list = 
  let show = (*show *) fun _ -> () in
  let ddn_final = fix_point compare_map_ddn show expand_map_ddn deps_list
  in StringMap.map ( fun (deps,_,_)-> deps ) ddn_final

(* FIN EXPANSION DE DEPENDANCES *)



(* PARTIES FORTEMENT CONNEXES *)
let get_deps deps node = try StringMap.find node deps with Not_found -> assert(false)

let get_strong_neigbours deps node =
  (
    (get_deps deps node)
  |> StringSet.filter (fun n-> StringSet.mem node (get_deps deps n) )
  |> StringSet.remove node
  )
;;

let get_many_strong_neigbours deps nodes =
  StringSet.fold (fun n acc -> StringSet.union (get_strong_neigbours deps n) acc  ) nodes nodes
;;

let get_connex_part deps node =
  fix_point StringSet.compare (fun _->()) (get_many_strong_neigbours deps) (StringSet.singleton node) ;;

(* FIN PARTIES FORTEMENT CONNEXES *)





(*
let connext_part_decomposition ddns = 
  let (dones,list_part) as acc = (StringSet.empty,[]) in
    StringMap.fold (fun v acc -> 
		      if not (StringSet.mem dones) then (
			let part= get_connex_part ddns v in
			  (StringSet. ,part:acc)
		      ) else acc
		   ) ddns
*)


let rec getTopoOrder1 deps cur (dones,order)=
  (*Printf.printf "\t\tGeneral : Try to add %s \n" cur;*)
  let (-) = StringSet.diff in
  let (+) = StringSet.union in
  if StringSet.mem cur dones then (dones,order)
  else 
    let undones_deps = (try (StringMap.find cur deps) with Not_found -> failwith "getTopoOrder1 ") - (dones + (StringSet.singleton cur)) in
      if StringSet.is_empty undones_deps 
      then (
	(*Printf.printf "\t All dependencies Ok, adding %s \n" cur;*)
	(StringSet.add cur dones  , cur::order)

      ) else (
	let (dones,order) = StringSet.fold (fun cur acc_order-> getTopoOrder1 deps cur acc_order) undones_deps (dones,order) in
	  getTopoOrder1 deps cur (dones,order)
      )
;;

(* ordre topologique à partir d'une liste de nom, permet de spécifier l'ordre des éléments *)
(* contient éventuellement des doublons *)
let getTopoOrder deps =
  (
    StringMap.fold (fun v _ acc -> 
		      (*Printf.printf "Primary : Try to add %s" v;*)
		      getTopoOrder1 deps v acc) deps (StringSet.empty,[])
  |> snd |> List.rev
  )
;;

let getTopoOrderOfList list deps =
  (
    List.fold_left (fun acc v -> (*Printf.printf "Primary : Try to add %s\n" v;*)getTopoOrder1 deps v acc) (StringSet.empty,[]) list
  |> snd |> List.rev
  )
;;





let contactSetName set = 
  if StringSet.cardinal set = 1 then  StringSet.choose set
  else
    let buf=Buffer.create 10 in
    let _ = Buffer.add_string buf "group" in
      StringSet.iter (fun n -> Buffer.add_string buf (toString "_%s" n)) set;
      (Buffer.contents buf) ;;



(*  *)
let majAccIncontructGroups deepDeps name _  (alreadyDones, groups, nameToGroupN)  =
  if StringSet.mem name alreadyDones then  (
    (*Printf.printf "Already done : %s \n" name;*)
    (alreadyDones, groups, nameToGroupN)

  )
  else (
    (*Printf.printf "Adding group of : %s \n" name;*)
    let (++) = StringSet.union in
      (* contenu du groupe *)
    let group = get_connex_part deepDeps name in
      (* nom du groupe, par défaut les fonctions du groupe *)
    let gN = contactSetName group in
      (* on ajoute la correspondance des noms du groupe à ce numéro de groupe *)
    let nameToGroupN = StringSet.fold (fun name acc -> StringMap.add name gN acc) group nameToGroupN in

      ( alreadyDones ++ group, StringMap.add gN group groups, nameToGroupN )

  )
;;

(* renvoie n groupe + les dépendances par groupe *)
(* avec ordre préférentiel de la liste *)
let contructGroups deepDeps val_list = 
  let (<+) m (k,v) = StringMap.add k v m in
  let init_state = (StringSet.empty,StringMap.empty,StringMap.empty) in
  let (_,groups,nameToGroupN) =
    StringMap.fold (majAccIncontructGroups deepDeps) deepDeps init_state
      (*List.fold_left (majAccIncontructGroups deepDeps) init_state name_list*)
  in
    (* Les groupes ... *)
    (*StringMap.iter (fun gname group->
		 Printf.printf "Group % s: " gname;
		 StringSet.iter (fun v->  Printf.printf " %s " v ) group;
		 Printf.printf "\n";
		 (*Printf.printf " depends on externals   : " ;
		   StringSet.iter (fun v->  Printf.printf " %s " v ) (StringSet.diff (get_deps deepDeps gname) group);
		   Printf.printf "\n";*)
		 Printf.printf " depends on Groups   : " ;
		 StringSet.iter (fun v->  Printf.printf " %s " v )  (StringSet.diff (mapStringSet (fun n-> StringMap.find n nameToGroupN) (get_deps deepDeps gname)) (StringSet.singleton gname));
		 Printf.printf "\n";
      ) groups ; *)
    (* Constructions des dépendances entre groupes *)

    let groupsDeps = StringMap.fold (fun gname _ acc -> 
				       (*Printf.printf "+ G %s\n" gname;*)
				       acc <+ (gname,
					       let fun_of_group =  (StringMap.find gname groups) |> StringSet.choose in
					       (get_deps deepDeps fun_of_group (*dépendance en terme de fonction*)
					       |>  mapStringSet (fun n-> StringMap.find n nameToGroupN)  (*vers dependance en terme de groupe*))
					      )
				    ) groups  StringMap.empty in
    let groupsOrder = val_list |>  List.map (fun n -> StringMap.find n nameToGroupN) in
    getTopoOrderOfList groupsOrder groupsDeps |> List.map (fun gname ->  (*Printf.printf "G%s\n" gname;*) gname, StringMap.find gname groups)
(*(get_deps deepDeps node)*)
;;
			   
			   

let show_deps deps =
  Printf.printf "-------------\n Dependances (format Makefile) : \n";
  List.iter (fun (n,ln) -> 
	       Printf.printf "%s : " n;
	       List.iter (Printf.printf "%s ") ln;
	       Printf.printf "\n"; 
	    ) deps;
  Printf.printf "------------- Fin Dependances\n"
;;

let show_deps_set deps =
  Printf.printf "-------------\n Dependances (format Makefile) : \n";
  List.iter (fun (n,ln) -> 
	       Printf.printf "%s : " n;
	       if StringSet.cardinal ln=0 then Printf.printf "NONE" else StringSet.iter (Printf.printf "%s ") ln;
	       Printf.printf "\n"; 
	    ) deps;
  Printf.printf "------------- Fin Dependances\n"
;;

let show_deps_expanded deps =
  Printf.printf "-------------\n Dependances (format Makefile) : \n";
  StringMap.iter (fun n ln -> 
	       Printf.printf "%s : " n;
	       if StringSet.cardinal ln=0 then Printf.printf "NONE" else StringSet.iter (Printf.printf "%s ") ln;
	       Printf.printf "\n"; 
	    ) deps;
  Printf.printf "------------- Fin Dependances\n"
;;





let create_group_list deps_set=
  let  ddns = init_set_ddns deps_set in
  let final = complete_expansion ddns in
  contructGroups final (List.map fst deps_set),final
;;

let show_groups gs= 
  List.iteri (fun (gname,group) i ->   
		Printf.printf "Groupe %d = Groupe %s = { " i gname;
		StringSet.iter (fun v->  Printf.printf " %s " v )  group;
		Printf.printf "}\n";
	   ) gs;;



(*
let test_deps = [
    ("A",["A";"B"]);
    ("B",["C";"E"]);
    ("C",["A"]);
    ("D",["C"]);
    ("E",[]);
    ("F",["D"])
  ]
;;

show_deps test_deps;;

let  test = init_ddns test_deps ;;
  



let final = complete_expansion test ;;

(*
StringMap.iter (fun node _ -> 
		  Printf.printf " %s :(Mergeable in \"let rec and\"  with): " node;
		  StringSet.iter (fun v->  Printf.printf " %s " v ) (get_strong_neigbours final node);
		  Printf.printf "\n";
		  Printf.printf " %s :(getConnexPart): " node;
		  StringSet.iter (fun v->  Printf.printf " %s " v ) (get_many_strong_neigbours final (StringSet.singleton node))   (*get_connex_part final node *);
		  Printf.printf "\n";
	       ) final ;;
*)

let gs = contructGroups final;;


List.iteri (fun (gname,group) i ->   
	      Printf.printf "Groupe %d = Groupe %s = { " i gname;
	      StringSet.iter (fun v->  Printf.printf " %s " v )  group;
	      Printf.printf "}\n";
	   ) gs;;





let depsListToGroups deps = init_ddns deps |> complete_expansion |>  contructGroups ;;
*)

		     
