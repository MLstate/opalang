(*
    Copyright © 2011, 2012 MLstate

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
(**

*)

(* depends *)
module List = BaseList


(* HACK : please, clean-up in opa lang *)
module Parser_utils = OpaParserUtils

open SurfaceAst
module C = SurfaceAstCons.StringCons
module I = Opacapi
let fresh_name = Parser_utils.fresh_name
let (!) = C.E.ident
let (&) = C.E.applys

let pattern_of_opt = function
  | None -> C.P.any ()
  | Some name -> C.P.ident name

let bind_of_opt = function
| None -> fun _ e -> e
| Some name -> C.E.letin name

let try_parse_opt () =
  C.E.dot !"Parser" "try_parse_opt"
let try_parse () =
  C.E.dot !"Parser" "try_parse"
let flatten_and_discard_whitespace_list () =
  C.E.dot !"Xml_parser" "flatten_and_discard_whitespace_list"


let value_pattern_p_k ?(optim_const=true) ?(keep_unused=true) ~name ?trp ?trx val_p =
  let mkstring (string,label) = C.P.string ~label string in
  let io, pl = val_p in
  let io = match io, pl with
  | Some ident, _ when keep_unused -> Some ident
  | _, [] -> None
  | _, [XmlValueConst _] when optim_const -> None (* special case when there is only one constant string to match, otherwise we will have to use a cascade of ifs. Not used for attributes *)
  | _ -> Some (fresh_name ~name ()) in
  let pat = match io, pl with
  | None, [XmlValueConst name] when optim_const -> mkstring name
  | Some ident, [XmlValueConst name] when optim_const -> C.P.as_ (mkstring name) ident
  | i, _ -> pattern_of_opt i in
  let rebind = match io, trx with
  | Some ident, Some trx -> fun k e -> k (C.E.letin ident (trx !ident) e)
  | _ -> Base.identity in
  let trp = Option.default Base.identity trp in
  let last_none = C.E.none () in
  let match_p ident eSucc p eFail =
    match p with
    | XmlValueConst (name,label)  -> C.E.if_ (!I.String.equals & [C.E.string ~label name; !ident]) eSucc eFail
    | XmlValueStringExpr se -> C.E.if_ (!I.String.equals & [trp se; !ident]) eSucc eFail
    | XmlValueStringParser sp ->
      C.E.match_opt (try_parse () & [!I.Parser.of_string & [sp]; !ident])
        (C.P.none (), eFail)
        (C.P.any (), eSucc)
    | XmlValueParserExpr pe ->
      C.E.match_opt (try_parse () & [pe; !ident])
        (C.P.none (), eFail)
        (C.P.any (), eSucc)
    | XmlValueParser items ->
      let item = List.hd items in
      let last_seq = if eFail == last_none then []
        else [({ Trx_ast.seq_items = []; Trx_ast.seq_code = Some eFail }, Parser_utils.nlabel item)] in
      let trx_expr = (Trx_ast.Expr
                        (({ Trx_ast.seq_items = items
                          ; Trx_ast.seq_code = Some eSucc },
                          Parser_utils.nlabel item)::last_seq), Parser_utils.nlabel item) in
      let p = fresh_name ~name:"p" () in
      C.E.letin p (SurfaceAstTrx.translate_rule trx_expr)
        (try_parse_opt () & [!p; !ident]) in
  let k k =
    let k = rebind k in
    match io, pl with
    | _, [] -> k
    | _, [XmlValueConst _] when optim_const -> k
    | None, _ -> assert false
    | Some ident, [p] -> fun e -> k (match_p ident e p last_none)
    | Some ident, pl ->
      let eSucc = C.E.some (C.E.void ()) in
      fun e ->
        let m = fresh_name ~name:("match_"^name) () in
        k (
          C.E.letin m (List.fold_right (match_p ident eSucc) pl last_none)
            (C.E.match_opt !m
               (C.P.none (), C.E.none ())
               (C.P.any (), e))
        ) in
  pat, k

let rec process_attribute attr content name =
  match attr with
  | XmlAttrMatch ({namespace = ns, []; name = n, []}, (value, [])) ->
    (* special case, any attribute will make it, just take the first one *)
    let p = match ns, n, value with
    | None, None, None -> C.P.any ()
    | _ -> C.P.coerce_name (C.P.record ["namespace",(pattern_of_opt ns); "name",(pattern_of_opt n); "value",(pattern_of_opt value)]) Opacapi.Types.Xml.attribute in
    let content = C.E.match_ !name
                   [ C.P.nil (), C.E.none ()
                   ; C.P.hd_tl p (C.P.ident name), content ] in
    content, name
  | XmlAttrMatch (nsp, vp) ->
    let rem_attr = fresh_name ~name:"rem_attr" () in
    let pns, kns = value_pattern_p_k ~optim_const:false ~name:"namespace" nsp.namespace in
    let pname, kname = value_pattern_p_k ~optim_const:false ~name:"name" nsp.name in
    let pvalue, kvalue = value_pattern_p_k ~optim_const:false ~name:"value" vp in
    let p = C.P.coerce_name (C.P.record ["namespace",pns; "name",pname; "value",pvalue]) Opacapi.Types.Xml.attribute in
    let k = kvalue (kns (kname Base.identity)) in
    let cb = fresh_name ~name:"cb" () in
    let e = k (C.E.letin name (!cb & []) content) in
    let f = C.E.lambda [p;C.P.var cb] e in
    let content = !I.List.find_map_cb & [f; !rem_attr] in
    content, rem_attr
  | XmlAttrPrefixed (XmlAnd, al) -> process_attributes name al content, name
  | XmlAttrPrefixed (XmlNot, al) ->
    let m = fresh_name ~name:"match" () in
    let content = C.E.letin m (process_attributes name al (C.E.some (C.E.void ())))
      (C.E.match_opt !m
         (C.P.none (), content)
         (C.P.any (), C.E.none ())) in
    content, name
  | XmlAttrSuffixed (i, al, suffixo) ->
    let content = match al with
    | [XmlAttrMatch ({namespace=None,[];name=None,[]},(None,[]))] -> (
      match suffixo with
      | None -> C.E.match_ !name
                 [ C.P.nil (), C.E.none ()
                 ; C.P.hd_tl (pattern_of_opt i) (C.P.ident name), content ]
      | Some Xml_star -> bind_of_opt i !name (C.E.letin name (C.E.nil ()) content)
      | Some Xml_plus -> C.E.match_ !name
                          [ C.P.nil (), C.E.none ()
                          ; (pattern_of_opt i), C.E.letin name (C.E.nil ()) content ]
      | Some Xml_question -> let q = fresh_name ~name:"question" () in
                             let v = C.E.match_ !name
                                      [ C.P.nil (), C.E.tuple_2 (C.E.none ()) (C.E.nil ())
                                      ; C.P.hd_tl (C.P.ident "hd") (C.P.ident "tl"), C.E.tuple_2 (C.E.some !"hd") !"tl"] in
                             C.E.letin q v
                               (C.E.match_ !q
                                  [ C.P.tuple_2 (pattern_of_opt i) (C.P.ident name), content ])
      | Some (Xml_number e) -> C.E.match_opt (C.E.applys !I.List.split_at_opt [!name;e])
                                (C.P.none (), C.E.none ())
                                (C.P.some (C.P.tuple_2 (pattern_of_opt i) (C.P.ident name)), content)
      | Some (Xml_range (e1, e2)) -> C.E.match_opt (C.E.applys !I.List.split_between [!name;e1;e2])
                                      (C.P.none (), C.E.none ())
                                      (C.P.some (C.P.tuple_2 (pattern_of_opt i) (C.P.ident name)), content)
      )
    | _ -> failwith "Not implemented: binding/suffixing of any attribute pattern" in
    content, name

and process_attributes (name:string) list content =
  if list = [] then content else
    let last_name = fresh_name ~name:"rem_attr" () in
    let content, last_name = List.fold_left (fun (content, name) attr ->
      process_attribute attr content name
    ) (content, last_name) list in
    C.E.letin last_name !name content

let error_suffix_anonymous_parser annot =
  let context = OpaError.Context.annot annot in
  OpaError.error context (
    "You cannot have a @{<bright>suffix@} on a tag.@\n"^^
    "@[<2>@{<bright>Hint@}:@\nPlease use an @{<bright>anonymous@} parser instead.@]@\n"
  )

let make_bind (name, expr) =
  if name = "" then
    C.E.record ["default", expr]
  else
    C.E.record ["name", C.E.string name; "uri" , expr]

let rec process_named_pattern env named_pattern l tl acc =
  match (named_pattern : _ xml_named_pattern) with
  | (name, XmlLetIn (bindings, subpattern), suffix) ->
      let bindings = C.E.list (List.map make_bind bindings) in
      let addpbind = C.E.ident (Opacapi.XmlParser.Env.add_pbinds) in
      C.E.letin env (C.E.applys addpbind [(C.E.ident env); bindings])
        (process_named_pattern env (name, subpattern, suffix) l tl acc)
  | (name, XmlAny, suffix) ->
      ( match suffix with
        | None ->
            C.E.match_ !l
              [ C.P.nil (), C.E.none ()
              ; C.P.hd_tl (pattern_of_opt name) (C.P.var tl), acc ]
        | Some (Xml_star,_) ->
            let acc = C.E.letin tl (C.E.nil ()) acc in
            bind_of_opt name !l acc
        | Some (Xml_plus,_) ->
            let acc = C.E.letin tl (C.E.nil ()) acc in
            C.E.match_ !l
              [ C.P.nil (), C.E.none ()
              ; C.P.hd_tl (C.P.any ()) (C.P.any ()),
                (match name with
                 | None -> acc
                 | Some name -> C.E.letin name !l acc)]
        | Some (Xml_question,_) ->
            let i = fresh_name ~name:"question" () in
            let v =
              C.E.match_ !l
                [ C.P.nil (), C.E.tuple_2 (C.E.none ()) (C.E.nil ())
                ; C.P.hd_tl (C.P.ident "hd") (C.P.ident "tl"), C.E.tuple_2 (C.E.some !"hd") !"tl"] in
            C.E.letin i v
              (C.E.match_ !i
                 [ C.P.tuple_2 (pattern_of_opt name) (C.P.ident tl), acc ])
        | Some (Xml_number e,_) ->
            C.E.match_opt (C.E.applys !I.List.split_at_opt [!l;e])
              (C.P.none (), C.E.none ())
              (C.P.some (C.P.tuple_2 (pattern_of_opt name) (C.P.ident tl)), acc)
        | Some (Xml_range (e1,e2),_) ->
            C.E.match_opt (C.E.applys !I.List.split_between [!l;e1;e2])
              (C.P.none (), C.E.none ())
              (C.P.some (C.P.tuple_2 (pattern_of_opt name) (C.P.ident tl)), acc)
      )
  | (name, XmlExpr e, suffix) ->
      let res =
        let e = !I.XmlParser.set_env & [e; !env] in
        match suffix with
        | None -> C.E.applys !I.XmlParser.raw_parse [e; !l]
        | Some (Xml_star,_) -> C.E.applys !I.Xml.match_star [e; !l]
        | Some (Xml_plus,_) -> C.E.applys !I.Xml.match_plus [e; !l]
        | Some (Xml_question,_) -> C.E.applys !I.Xml.match_question [e; !l]
        | Some (Xml_number e1,_) -> C.E.applys !I.Xml.match_number [e; e1; !l]
        | Some (Xml_range (e1,e2),_) -> C.E.applys !I.Xml.match_range [e; e1; e2; !l] in
      C.E.match_opt res
        (C.P.none (), C.E.none ())
        (C.P.some (C.P.tuple_2 (pattern_of_opt name) (C.P.ident tl)), acc)
  | (name,XmlNode (nstag,attr,children),None) -> (
    let attrs = fresh_name ~name:"attrs" () in
    let args = fresh_name ~name:"args" () in
    let xmlns = fresh_name ~name:"xmlns" () in
    let trp e = !I.XmlParser.Env.p_get_uri & [!env; e] in
    let trx e = !I.XmlParser.Env.x_get_uri & [!env; e] in
    let pns, kns = value_pattern_p_k ~name:"ns" ~trp ~trx nstag.namespace in
    let ptag, ktag = value_pattern_p_k ~name:"tag" nstag.name in
    let p_hd = (C.P.coerce_name
               (C.P.record [ "namespace", pns
                           ; "tag", ptag
                           ; "args", (if attr = [] then C.P.any () else C.P.var attrs)
                           ; "content", (if children = [] then C.P.any () else C.P.var args)
                           ; "specific_attributes", C.P.any ()
                           ; "xmlns", C.P.var xmlns
                           ])
               Opacapi.Types.xml) in
    let p_hd = match name with
    | Some ident -> C.P.as_ p_hd ident
    | None -> p_hd in
    let k e =
      C.E.match_ !l
        [ C.P.hd_tl p_hd (C.P.ident tl), e
        ; C.P.any (), C.E.none () ] in
    let k = ktag k in
    let k e =
      k (
        C.E.letin env
          (!I.XmlParser.Env.add_xbinds & [C.E.ident env; C.E.ident xmlns])
          e
      ) in
    let k = kns k in
    k (
      let acc = if children = [] then acc else
        let last_name = fresh_name ~name:"dontcare" () in
        C.E.letin args (flatten_and_discard_whitespace_list () & [!args])
          (process_named_patterns args env children last_name acc) in
      process_attributes attrs attr acc
    )
  )
  | (_, XmlNode _, Some (_suffix,annot)) -> (
          (* instance of error:  xml_parser <mlk> <mlk/>* </> -> {}
           * happens because in xml_parser <mlk> <mlk a=_/>* </>, what should be the type of a?
           * each nesting inside a star/plus/... could create a list, but it isn't
           * done and it hasn't been asked for *)
          error_suffix_anonymous_parser annot
  )
  | (_, XmlParser _, Some (_suffix,annot)) ->
      (* same problem as above, XmlParser may bind variables *)
      error_suffix_anonymous_parser annot
  | (name, XmlParser items, None) ->
     assert (name = None); (* see the parser *)
      let item = List.hd items in
      let trx_expr =
        (Trx_ast.Expr
           [({ Trx_ast.seq_items = items
             ; Trx_ast.seq_code = Some acc },
             Parser_utils.nlabel item)], Parser_utils.nlabel item) in
      let p = fresh_name ~name:"p" () in
      let res = fresh_name ~name:"res" () in
      C.E.match_ !l
        [ C.P.hd_tl
            (C.P.coerce_name (C.P.record ["text", C.P.var res]) Opacapi.Types.xml)
            (C.P.var tl),
          C.E.letin p (SurfaceAstTrx.translate_rule trx_expr)
            (try_parse_opt () & [C.E.var p; C.E.var res])
        ; C.P.any (), C.E.none () ]
  | (name, XmlPrefixed (XmlAnd, npl), suffix) ->
    assert (name = None);
    assert (suffix = None);
    let last_name = fresh_name ~name:"last_name" () in
    let res = C.E.letin tl !l acc in
    process_named_patterns l env npl last_name res
  | (name, XmlPrefixed (XmlNot, npl), suffix) ->
    assert (name = None);
    assert (suffix = None);
    let m = fresh_name ~name:"match" () in
    let last_name = fresh_name ~name:"last_name" () in
    let res = C.E.some (C.E.void ()) in
    let stop = C.E.none () in
    let cont = C.E.letin tl !l acc in
    C.E.letin m (process_named_patterns l env npl last_name res)
      (C.E.match_opt !m
         (C.P.none (), cont)
         (C.P.any (), stop))

and process_named_patterns xml env named_patterns last_name e : (_,_) expr =
  let acc, _ =
  List.fold_right_i
    (fun (named_pattern : _ xml_named_pattern) i (acc,tl) ->
       let l = if i = 0 then xml else fresh_name ~name:"l" () in
       (process_named_pattern env named_pattern l tl acc,l)
    ) named_patterns (e,last_name) in
  acc

(* FIXME: imcompatible patterns could be merged in one pattern
 * xml_parser
 * | <mlk/> -> ...
 * | <poi/> -> ...
 * could be compiled to
 * match xmls with
 * | [<mlk/>|rest] -> ...
 * | [<poi/>|rest] -> ... /* no backtracking possible between those two cases */
 * | _ -> ...
 *)
let process_rule xml env (patterns,e) : (_,_) expr =
  let last_name = fresh_name ~name:"last_name" () in
  let res = C.E.some (C.E.tuple_2 e (C.E.ident last_name)) in
  process_named_patterns xml env patterns last_name res

let process_rules xml env rules =
  let l, def = rules in
  let last_none = C.E.none () in
  let def = match def with
  | None -> last_none
  | Some e -> C.E.some (C.E.tuple_2 e !xml) in
  List.fold_right_i
    (fun rule_ i acc ->
       if acc == last_none then
         process_rule xml env rule_ (* avoid a stupid match *)
       else
         let n = fresh_name ~name:(Printf.sprintf "case_%d" i) () in
         C.E.letin n (process_rule xml env rule_)
           (C.E.match_opt !n
              (C.P.none (), acc)
              (C.P.ident "res", !"res"))) l def

let process_parser _e rules =
  #<If:SA_XML_PATTERN>
    Format.printf "%a@." OpaPrint.string#expr _e
  #<End>;
  let xml = fresh_name ~name:"xml" () in
  let env  = fresh_name ~name:"env"  () in
  let body = process_rules xml env rules in
  let p = !I.XmlParser.make & [(C.E.lambda [C.P.ident xml; C.P.ident env] body)] in
  C.D.nonexpansive p
