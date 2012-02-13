(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(** Wsdl2mlCommon:
    Common code between wsdl2ml translator and generated code runtime.
*)

module List = Base.List
module String = Base.String

let eprintf = Printf.eprintf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf

(* Tools used by the wsdl2ml translator *)

type tree = E of Xmlm.tag * tree list | D of string

let sname ((ns,name):Xmlm.name) = ns^(if ns <> "" then ":" else "")^name
let satt ((name,str):Xmlm.attribute) = sname name^"=\""^str^"\""
let satts atts = String.sconcat ~left:"[" ~right:"]" "; " (List.map satt atts)
let stag ((name,atts):Xmlm.tag) = sname name^(String.concat ";\n" (List.map satt atts))
let gtag = function | E (tag,_) -> tag | D _ -> raise (Failure "no tag")
let gts = function | E (_,trees) -> trees | D _ -> raise (Failure "no trees")

let mkname ns name = ((ns,name):Xmlm.name)
let mkatt name str = ((name,str):Xmlm.attribute)
let mktag name atts = ((name,atts):Xmlm.tag)
let mkstag name = mktag (mkname "" name) []

let in_tree i = 
  let el tag children =
    (*eprintf "in_tree: tag=%s\n" (stag tag); flush stderr;*)
    E (tag, children)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t = 
  let frag = function
  | E (tag, children) -> `El (tag, children) 
  | D d -> `Data d 
  in
  Xmlm.output_doc_tree frag o t

let string_of_tree ?(hint=1024) t =
  let b = Buffer.create hint in
  let o = Xmlm.make_output ~indent:(Some 2) (`Buffer b) in
  out_tree o t;
  Buffer.contents b

let sxml xml = string_of_tree (None,List.hd xml)

let is_name ((ns1,name1):Xmlm.name) ((ns2,name2):Xmlm.name) = ns1 = ns2 && name1 = name2
let is_uqname ((_,name1):Xmlm.name) ((_,name2):Xmlm.name) = name1 = name2

let find_tag is_tag trees = List.find_opt (function | (E (tag,_)) -> is_tag tag | D _ -> false) trees

let find_att (name:Xmlm.name) (atts:Xmlm.attribute list) =
  match List.find_opt (function (n,_) -> is_uqname name n) atts with
  | Some (_,att) -> Some att
  | None -> None

let get_tree_string content =
  let src = `String (0,content) in
  let i = Xmlm.make_input ~strip:true src in
  let t = in_tree i in
  (*eprintf "xml: %s\n%!" (string_of_tree t);*)
  t

let get_tree_filename filename = get_tree_string (File.content filename)

let null_fold_tree_f = function acc -> function E (_tag, _trees) -> acc | D _str -> acc

let fold_tree f def tree =
  let rec aux acc = function
    | E (tag, trees) ->
        let acc = f acc (E (tag, trees)) in
        List.fold_left aux acc trees
    | D str ->
        f acc (D str)
  in
  aux def tree

let fold_trees f def trees = List.fold_left (fun acc (_ns,(_dtd,tree)) -> fold_tree f acc tree) def trees

let find_trees f trees = List.fold_left (fun acc (_ns,(_dtd,tree)) -> if f tree then tree::acc else acc) [] trees

(* Implementations of types found in XML Schema *)

(* ws:dateTime *)

(*'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?*)

type t_dateTime = Time.t

let string_of_dateTime (dT:t_dateTime) =
  let _msec = Time.gmt_msec dT in
  let sec = Time.gmt_sec dT in
  let min = Time.gmt_min dT in
  let hour = Time.gmt_hour dT in
  let mday = Time.gmt_mday dT in
  let mon = Time.gmt_mon dT in
  let year = Time.gmt_year dT in
  (*let wday = Time.gmt_wday dT in
  let yday = Time.gmt_yday dT in
  let isdst = Time.gmt_isdst dT in*)
  sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" year (mon+1) mday hour min sec _msec
  (*sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" year (mon+1) mday hour min sec*)

let dTre = Str.regexp "-?\\([0-9][0-9][0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)T\\([0-2][0-9]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\)\\(\\(\\.\\([0-9]+\\)\\)?\\)\\(\\(\\(\\([+-]\\)\\([0-5][0-9]\\):\\([0-5][0-9]\\)\\)\\|\\(Z\\)\\)?\\)"

(* FIXME : this is complete rubbish (Timezones are wrong) *)

let dateTime_of_string str : t_dateTime =
  if Str.string_match dTre str 0
  then
    let yyyy = Str.matched_group 1 str in
    let mmmm = Str.matched_group 2 str in
    let dddd = Str.matched_group 3 str in
    let hh = Str.matched_group 4 str in
    let mm = Str.matched_group 5 str in
    let ss = Str.matched_group 6 str in
    let msec = try Str.matched_group 9 str with Not_found -> "000" in
    let pm, zhh, zmm =
      try Str.matched_group 13 str, Str.matched_group 14 str, Str.matched_group 15 str
      with Not_found -> "Z", "00", "00" in
    (*yyyy, mmmm, dddd, hh, mm, ss, msec, pm, zhh, zmm*)
    let conv s mn mx =
      let i = try int_of_string s with Failure "int_of_string" -> raise (Failure ("dateTime_of_string "^s)) in
      if i >= mn && i <= mx then i else raise (Failure ("dateTime_of_string "^s))
    in
    let year = conv yyyy 0 3000 in
    let month = conv mmmm 1 12 in
    let day = conv dddd 1 31 in
    let h = conv hh 0 24 in
    let min = conv mm 0 60 in
    let sec = conv ss 0 60 in
    let ms = conv msec 0 999 in
    (*eprintf "start=%02d:%02d\n%!" h min;*)
    let bt = Time.mktime ~year ~month ~day ~h ~min ~sec ~ms in
    (*eprintf "start(bt,gmt): %02d:%02d\n%!" (Time.gmt_hour bt) (Time.gmt_min bt);*)
    (*eprintf "start(bt,local): %02d:%02d\n%!" (Time.local_hour bt) (Time.local_min bt);*)
    if pm = "Z"
    then bt
    else
      let zhour = conv zhh 0 24 in
      let zmin = conv zmm 0 60 in
      (*eprintf "diff: %s%02d:%02d\n%!" pm zhour zmin;*)
      let t = Time.add (Time.hours zhour) (Time.minutes zmin) in
      (*eprintf "diff(t,gmt): %02d:%02d\n%!" (Time.gmt_hour t) (Time.gmt_min t);*)
      (*eprintf "diff(t,local): %02d:%02d\n%!" (Time.local_hour t) (Time.local_min t);*)
      match pm with
      | "+" -> Time.difference t bt
      | "-" -> Time.add bt t
      | _ -> raise (Failure ("dateTime_of_string: Unknown time modifier "^str))
  else
    raise (Failure ("dateTime_of_string "^str))

(*
let str2 = "2011-04-02T07:22:59.138+02:00";;
let str3 = "2002-10-10T12:00:00+05:00";;
let str4 = "2011-04-02T07:22:59.138Z";;
let t = dateTime_of_string str4;;
let _ = eprintf "final(gmt): %02d:%02d\n%!" (Time.gmt_hour t) (Time.gmt_min t);;
let _ = eprintf "final(local): %02d:%02d\n%!" (Time.local_hour t) (Time.local_min t);;
*)

(* ws:byte *)

type t_byte = int

let chk_byte name b = if b < -127 || b > 128 then raise (Failure (sprintf "%s value out of range" name))

let string_of_byte (b:t_byte) =
  chk_byte "string_of_byte" b;
  string_of_int b

let byte_of_string str : t_byte =
  try
    let b = int_of_string str in
    chk_byte "byte_of_string" b;
    b
  with Failure "int_of_string" -> raise (Failure "byte_of_string")

(* Exceptions generated by generated code *)

exception Wsdl2mlOccurs of int * int * tree list
exception Wsdl2mlNonMtchCon of string
exception Wsdl2mlInputFailure of string

(* Generic converters to be used to fill in blanks created by <any/> *)

let toxml_string s = [D s]
let toxml_int i = [D (string_of_int i)]
let toxml_byte b = [D (string_of_byte b)]
let toxml_float f = [D (string_of_float f)]
let toxml_bool b = [D (string_of_bool b)]
let toxml_dateTime dT = [D (string_of_dateTime dT)]

let fx n os v = try os v with Failure nn when (n^"_of_string") = nn -> raise (Wsdl2mlInputFailure (sprintf "Expected %s" n))

let fromxml_string = function [D s] -> s | _ -> raise (Wsdl2mlInputFailure "Expected string")
let fromxml_int = function [D i] -> fx "int" int_of_string i | _ -> raise (Wsdl2mlInputFailure "Expected int")
let fromxml_byte = function [D b] -> fx "byte" byte_of_string b | _ -> raise (Wsdl2mlInputFailure "Expected byte")
let fromxml_float = function [D f] -> fx "float" float_of_string f | _ -> raise (Wsdl2mlInputFailure "Expected float")
let fromxml_bool = function [D b] -> fx "bool" bool_of_string b | _ -> raise (Wsdl2mlInputFailure "Expected bool")
let fromxml_dateTime = function [D dT] -> fx "dateTime" dateTime_of_string dT | _ -> raise (Wsdl2mlInputFailure "Expected dateTime")

(* Tools used internally by generated code *)

let find_name name sts =
  List.fold_left (fun acc -> function E (((_,n),_),_) as e -> if n = name then e::acc else acc | _ -> acc) [] sts

let find_names names sts =
  List.fold_left (fun acc -> function E (((_,n),_),_) as e -> if List.mem n names then e::acc else acc | _ -> acc) [] sts

(* Some support for digging around in the XML *)

let get_sts name sts =
  (function
   | (E (((_,n),_), (sts)))::_ when n = name -> sts
   | (E (((_,n),_), _))::_ -> raise (Failure ("is actually: "^n))
   | (D str)::_ -> raise (Failure ("is actually D: "^str))
   | [] ->  raise (Failure "is actually []"))
  (find_name name sts)

let sts_names sts = List.map (function
   | (E (((_,n),_), _)) -> n
   | (D str) -> ("D:"^str)) sts

let dig_sts names sts =
  let rec aux sofar sts = function
    | name::names ->
        (try
           aux (sofar@[name]) (get_sts name sts) names
         with _ -> (sofar,sts))
    | [] -> (sofar,sts)
  in
  aux [] sts names

let is_tree_name name = function
  | (E (((_,n),_), _)) -> n = name
  | (D _) -> false

(* End of file wsdl2mlCommon.ml *)



