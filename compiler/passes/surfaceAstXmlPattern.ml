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

let try_parse_opt () =
  C.E.dot !"Parser" "try_parse_opt"
let try_parse () =
  C.E.dot !"Parser" "try_parse"
let flatten_and_discard_whitespace_list () =
  C.E.dot !"Xml_parser" "flatten_and_discard_whitespace_list"

(*
let may_be_compatible patterns1 patterns2 =
  match patterns1, patterns2 with
  | [], _
  | _, [] -> assert false (* cannot parse empty sequences *)
  | p1 :: _, p2 :: _ ->
      match p1, p2 with
      | XmlNode _, XmlParser _ -> false
      | XmlParser _, XmlNode _ -> false
      | XmlNode ({namespace=(ns1,_);name=(n1,_)},_,_),
        XmlNode ({namespace=(ns2,_);name=(n2,_)},_,_) ->
          ns1 = ns2 && n1 = n2
      | XmlAny, _
      | _, XmlAny
      | XmlExpr _, _
      | _, XmlExpr _ -> true
      | XmlParser _, XmlParser _ ->
          true (* FIXME: actually, we should regroup these two parsers
                * into one, so that trx can compile it possibly in a smarter way *)
*)

let process_attribute name (attr,name_opt,attr_check) content =
  let expr = !I.Xml.find_attr & [!name; attr.namespace; Parser_utils.string2 attr.name] in
  let bound_ident =
    match name_opt with
    | None -> C.P.ident ~label:(snd attr.name) (fst attr.name)
    | Some bound_name -> C.P.ident bound_name in
  let attrParser parser_ =
    let val_name = Parser_utils.fresh_name ~name:"value" () in
    let match_attr content =
      C.E.match_opt expr
        (C.P.none (), C.E.none ())
        (C.P.some (C.P.ident val_name), content) in
    let match_attr_val =
      C.E.match_opt (try_parse () & [parser_; !val_name])
        (C.P.none (), C.E.none ())
        (C.P.some bound_ident, content) in
    match_attr match_attr_val
  in
  match attr_check with
    | XmlAttrStringParser se ->
        (* convert the string to a parser (no more magic for that) *)
        attrParser (!I.Parser.of_string & [se])
    | XmlAttrParser parser_ -> attrParser parser_
    | XmlExists ->
        C.E.match_opt expr
          (C.P.none (), C.E.none ())
          (C.P.some (C.P.any ()), content)
    | XmlName ->
        C.E.match_opt expr
          (C.P.none (), C.E.none ())
          (C.P.some bound_ident, content)

let process_attributes (name:string) list content =
  List.fold_right (fun attr_node acc ->
                    process_attribute name attr_node acc
                  ) list content

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
            ( match name with
              | None -> acc
              | Some name -> C.E.letin name !l acc
            )
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
            C.E.match_opt (C.E.applys !I.List.split_at_opt [e;!l])
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
  | (_name,XmlNode (nstag,attr,children),None) -> (
    let mkstring (string,label) = C.P.string ~label string in
    let name_pattern_p_k ~name ?trp ?trx name_p =
      let io = match name_p with
      | Some ident, _ -> Some ident
      | _, (XmlNameConst _ | XmlNameAny) -> None
      | _ -> Some (fresh_name ~name ()) in
      let _, p = name_p in
      let pat = match io, p with
      | None, XmlNameConst name -> mkstring name
      | Some ident, XmlNameConst name -> C.P.as_ (mkstring name) ident
      | None, _ -> C.P.any ()
      | Some ident, _ -> C.P.var ident in
      let reb = match io, trx with
      | Some ident, Some trx -> fun k e -> k (C.E.letin ident (trx !ident) e)
      | _ -> Base.identity in
      let trp = Option.default Base.identity trp in
      let k k =
        let k = reb k in
        match io, p with
        | _, (XmlNameConst _ | XmlNameAny) -> k
        | None, _ -> assert false
        | Some ident, XmlNameStringExpr se ->
          fun e -> k (
            C.E.if_ (!I.String.equals & [trp se; !ident])
              e
              (C.E.none ())
          )
        | Some ident, XmlNameParserExpr pe ->
          fun e -> k (
            C.E.match_opt (try_parse () & [pe; !ident])
              (C.P.none (), C.E.none ())
              (C.P.any (), e)
          )
        | Some ident, XmlNameParser items ->
          let item = List.hd items in
          fun e ->
            let trx_expr =
              (Trx_ast.Expr
                 [({ Trx_ast.seq_items = items
                   ; Trx_ast.seq_code = Some e },
                   Parser_utils.nlabel item)], Parser_utils.nlabel item) in
            let p = fresh_name ~name:"p" () in
            k (
              C.E.letin p (SurfaceAstTrx.translate_rule trx_expr)
                (try_parse_opt () & [!p; !ident])
            ) in
      pat, k in
    let attrs = fresh_name ~name:"attrs" () in
    let args = fresh_name ~name:"args" () in
    let xmlns = fresh_name ~name:"xmlns" () in
    let trp e = !I.XmlParser.Env.p_get_uri & [!env; e] in
    let trx e = !I.XmlParser.Env.x_get_uri & [!env; e] in
    let pns, kns = name_pattern_p_k ~name:"ns" ~trp ~trx nstag.namespace in
    let ptag, ktag = name_pattern_p_k ~name:"tag" nstag.name in
    let k e =
      C.E.match_ !l
        [ C.P.hd_tl (
          C.P.coerce_name
            (C.P.record [ "namespace", pns
                        ; "tag", ptag
                        ; "args", (if attr = [] then C.P.any () else C.P.var attrs)
                        ; "content", (if children = [] then C.P.any () else C.P.var args)
                        ; "specific_attributes", C.P.any ()
                        ; "xmlns", C.P.var xmlns
                        ])
            Opacapi.Types.xml
          ) (C.P.ident tl), e
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
