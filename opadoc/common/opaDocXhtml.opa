/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * {1 opadoc Xhtml common library}
**/

OpaDocXhtml = {{

  /**
   * The default link map is empty, it does not generate hyperlink
  **/
  default_link_map = StringMap.empty : Join.Html.link.map

  @private make_href(link_elt : Join.Html.link.elt) =
    fname = link_elt.fname
    path_html = link_elt.path_html
    "{fname}.html#{path_html}"

  @private make_hyperlink(link : Join.Html.link.map)(path : string, body : xhtml) =
    match StringMap.get(path, link) with
    | { none } -> body
    | { some = link_elt } ->
      href = make_href(link_elt)
      <a href="{href}">{body}</a>

  /**
   * Pretty print an OpaType.ty in xhtml format
   */
  opaty_to_xhtml(hyperlink:Join.Html.link.map, ty:Api.ty) : xhtml =
    opaty_to_xhtml_internal(hyperlink, {none}, ty)

  /**
   * The path is the location of the type for adding e.g. hyperlink to fields definitions
  **/
  opaty_to_xhtml_in_path(hyperlink:Join.Html.link.map, path : string, ty:Api.ty) : xhtml =
    opaty_to_xhtml_internal(hyperlink, {some=path}, ty)

  string_of_code_elt: Api.code_elt -> string =
    | {value = _} -> "value"
    | {type_def = _} -> "type"

  @private
  greek_entities = [
      <>&alpha;</>,
      <>&beta;</>,
      <>&gamma;</>,
      <>&delta;</>,
      <>&epsilon;</>,
      <>&zeta;</>,
      <>&eta;</>,
      <>&theta;</>,
      <>&iota;</>,
      <>&kappa;</>,
      <>&lambda;</>,
      <>&mu;</>,
      <>&nu;</>,
      <>&xi;</>,
      <>&omicron;</>,
      <>&pi;</>,
      <>&rho;</>,
      <>&sigmaf;</>,
      <>&sigma;</>,
      <>&tau;</>,
      <>&upsilon;</>,
      <>&phi;</>,
      <>&chi;</>,
      <>&psi;</>,
      <>&omega;</>,
    ]

  /*
   * Convert a single variable name of the form ['vXX] (where X is a number) to 
   * a greek letter using an alphabet
   */
  @private
  entity_of_var(alpha: list(xhtml), var: string): xhtml =
    default_name = <>{var}</>
    var_parser = parser "'v" n=Rule.integer -> n
    match Parser.try_parse(var_parser, var) with
      | {none} -> default_name
      | {~some} -> List.get(some, alpha) ? default_name

  /*
   * Replace the beginning of a list of anonymous vars (of the form ['vXX] 
   * where XX is a number) by greek letters (represented as HTML entities).
   */
  @private
  to_greek_entities(vars: list(string)): list(xhtml) =
    List.map(entity_of_var(greek_entities, _), vars)

  @private
  concat_xhtml(separator: xhtml, elements: list(xhtml)): xhtml =
    match elements with
      | [] -> <></>
      | [hd|tl] -> List.fold((elt, acc -> <>{acc}{separator}{elt}</>), tl, <>{hd}</>)

  @private
  concat_greek(vars: list(string)): xhtml =
    concat_xhtml(<>, </>, to_greek_entities(vars))

  @private opaty_to_xhtml_internal(hyperlink:Join.Html.link.map, path : option(string), ty:Api.ty) : xhtml =
    with_link = make_hyperlink(hyperlink)
    // Imperative gestion of path, push, pop
    path_mutable = Mutable.make([ path ? "" ])
    link_field = Option.is_some(path)
    pop_field() =
      match path_mutable.get() with
      | [] -> void
      | [ _ | tl ] -> path_mutable.set(tl)
    path_field(field) =
      match path_mutable.get() with
      | [] -> field
      | [hd | _] ->
        "{hd}.{field}"
    push_field(field) =
      path = path_field(field)
      list = path_mutable.get()
      path_mutable.set([path | list ])
    // End of path gestion
    aux_tuple(id) =
      if (String.length(id) > 6) && (String.substring(0,6,id) == "tuple_") then ""
      else id
    rec aux_ty_const(ty_const) =
      match ty_const with
      | {TyInt} -> <>int</>
      | {TyFloat} -> <>float</>
      | {TyString} -> <>string</>
    and aux_list_sugar(list) =
      elems = List.fold((ty, (acc, pre) ->
        (<>{acc}{pre}{aux(ty)}</>, <> / </>)),
        list, (<></>, <></>)).f1
      elems
    and aux_list(list:list(OpaType.ty)) =
      elems = List.fold((ty, (acc, pre) ->
        (<>{acc}{pre}{aux(ty)}</>, <>, </>)),
        list, (<></>, <></>)).f1
      elems
    and aux_fields(fields) = List.fold((field, (acc, pre) ->
      (
        fieldname = OpaType.Field.to_string(field.label)
        aux_ty =
          if link_field
          then
            do push_field(fieldname)
            aux_ty = aux(field.ty)
            do pop_field()
            aux_ty
          else aux(field.ty)
        fieldname = (
          if link_field
          then
            with_link(path_field(fieldname), <>{fieldname}</>)
          else <>{fieldname}</>
        )
        <>{acc}{pre}{fieldname} : {aux_ty}</>, <>;<br /></>
      )),
      fields, (<></>, <></>)).f1
    and aux_fields_list(lfields) = List.fold((field, (acc, pre) ->
      (<>{acc}{pre} \{{aux_fields(field)}}</>, <><br /> / </>)),
      /*lfields, (<></>, <></>)).f1*/
      lfields, (<><br /></>, <></>)).f1
    and aux(ty:OpaType.ty) =
      match ty with
      | {TyConst=x} -> <>{aux_ty_const(x)}</>
      | {~TyVar} -> entity_of_var(greek_entities, TyVar)
      | {~TyArrow_params ~TyArrow_res} ->
        <>({aux_list(TyArrow_params)} <span class="rarr">&rarr;</span> {aux(TyArrow_res)})</>
      | {~TyRecord_row} -> <>\{{aux_fields(TyRecord_row)}}</>
      | {~TyRecord_row ~TyRecord_rowvar} ->
        <>\{{aux_fields(TyRecord_row)}; {entity_of_var(greek_entities, TyRecord_rowvar)}}</>
      | {~TySum_col} -> aux_fields_list(TySum_col)
      | {~TySum_col ~TySum_colvar} ->
        <>{aux_fields_list(TySum_col)} / {entity_of_var(greek_entities, TySum_colvar)}</>
      | {TyName_args=[] ~TyName_ident} -> with_link(TyName_ident, <>{TyName_ident}</>)
      | {~TyName_args ~TyName_ident} ->
        r = with_link(TyName_ident, <>{aux_tuple(TyName_ident)}</>)
        <>{r}({aux_list(TyName_args)})</>
      | {TyAbstract} -> <>external</>
      | {~TyForall_quant ~TyForall_body} ->
        <>&forall;
          {concat_greek(TyForall_quant.types
          ++ TyForall_quant.rows
          ++ TyForall_quant.cols)}.{aux(TyForall_body)}
        </>
      end
    aux_arrow(ty) =
      match ty with
      | {~TyArrow_params ~TyArrow_res} ->
        <>{aux_list(TyArrow_params)} <span class="rarr">&rarr;</span> {aux(TyArrow_res)}</>
      | _ -> <>{aux(ty)}</>
      end
    aux_sugar(ty) =
      match ty with
      | {~TySumSugar} -> <>{aux_list_sugar(TySumSugar)}</>
      | _ -> <>{aux_arrow(Magic.id(ty):OpaType.ty)}</>
      end
    <>{aux_sugar(ty)}</>

   /**
    * Pretty print of an opa type definition
    */
   type_def_to_xhtml(hyperlink:Join.Html.link.map, type_def : Api.type_def) : xhtml =
     TyName_args = List.map(v->{TyVar=v}, type_def.ty_def_params)
     TyName_ident = type_def.ty_def_name
     type_ = ~{ TyName_args TyName_ident } : Api.ty
     // FIXME: see parameters,
     // a global ref would be OK for this server side application
     is_public =
       match type_def.ty_def_visibility with
       | { TDV_public } -> true
       | _ ->
         params = OpaDocParameters.get()
         // If we are in private mode, everything is published for internal developpement
         params.private

     if is_public
     then
       body = type_def.ty_def_body
       <>
         <code class="type_def">type {
           // do not hyperlink the type to itself
           opaty_to_xhtml(default_link_map, type_)
         } = </code>
         <code class="type">{opaty_to_xhtml(hyperlink, body)}</code>
       </>
     else
       // if it is asked by someone, we can add a abstract keyword somewhere
       <code class="type_def">type {
         // do not hyperlink the type to itself
         opaty_to_xhtml(default_link_map, type_)
       }</code>

  to_xhtml(hyperlink:Join.Html.link.map, res : OpaDocTy.txt(string), mode:Comment.mode) =
    with_link = make_hyperlink(hyperlink)

    append((buf: xhtml, acc: xhtml), content: xhtml) =
      (<>{buf}{content}</>, acc)

    flush((buf: xhtml, acc: xhtml), transform: (xhtml -> xhtml)): (xhtml, xhtml) =
      if buf == <></> then
        (buf, acc)
      else
        (<></>, <>{acc}{transform(buf)}</>)

    _flush_para(acc: (xhtml, xhtml), classes: list(string)): (xhtml, xhtml) =
      wrap_para(content: xhtml) =
        <p class="{String.concat(", ", classes)}">{content}</p>
      flush(acc, wrap_para)

    flush_id(acc) = flush(acc, identity)

    flush_line(acc: (xhtml, xhtml)): (xhtml, xhtml) =
      new_line(content: xhtml) =
        <><br />{content}</>
      flush(acc, new_line)

    append_body(acc, content) =
      append(acc, content)
        |> flush_id(_)

    rec aux({l = l}) =
      List.fold_left((acc, (x : OpaDocTy.txt_element(string) /* FIX Unknown[anyToXml] */ ) ->
        match x with
          | { Esc_accol } -> error("assert false")
          | { Esc_croch } -> error("assert false")

          | ~{Raw} -> <span>{Raw}</span>
            |> append_body(acc, _)

          | { License = (d1, d2_opt) } ->
            d2 = Option.switch(d2 -> "-" ^ "{d2}", "", d2_opt)
            (match mode with
            | {Raw} -> mlstate = (<a title="MLstate website" href="http://mlstate.com">MLstate</a>)
              <div>
              { OpaDocUtils.fold_xhtml((x -> <>{x}<br /></>),
              [ <span class="copy">&copy; {d1}{d2} {mlstate}</span>
              , <span class="lic">All rights reserved.</span>
              , <span class="lic">This file is confidential and intended solely for the addressee(s).</span>
              , <span class="lic">Any unauthorized use or dissemination is prohibited.</span> ])}</div>
            | {Doc} -> <></>)
              |> append_body(acc, _)

          | { Authors = l } ->
            (match mode with
            | {Raw} ->
              fold_nl(f, l) =
              match l with
                | [] -> <></>
                | _ -> (x, xs) = l = List.rev(l) (List.head(l), List.tail(l))
                <>{OpaDocUtils.fold_xhtml((s -> <>{f(s)}<br /></>), List.rev(xs))}{f(x)}</>
            fold_nl((s -> <><span class="s1">authors</span> <span class="author">{s}</span></>), l)
            | {Doc} -> <></>)
              |> append_body(acc, _)

          | { Param = (i, s) } ->
            x = flush_id(aux({l = s})).f2
            <>
              <br />
              <span class="s1">param</span>
              <span class="param">{i}</span>
              {x}
            </>
              /*|> append_close_para(aux({l = s}), _, ["arobase_wrapper"])*/
              /*|> append_body(append_body(acc, flush_id(aux({l = s})).f2), _)*/
              |> append_body(acc, _)

          | { Return = s } ->
            x = flush_id(aux({l = s})).f2
            <>
              <br />
              <span class="s1">return</span>
              {x}
            </>
              /*|> append_close_para(aux({l = s}), _, ["arobase_wrapper"])*/
              |> append_body(acc, _)

          | { Other_arobase = (name, l, s) } ->
            x = flush_id(aux({l = s})).f2
            (match mode with
            | {Raw} ->
              <>
                <br />
                <span class="s1">{name}</span>
                <span class="annotation">{List.to_string_using("", "", " ", l)}</span>
                {x}
              </>
            | {Doc} -> <>{x}</>)
              /*|> append_close_para(aux({l = s}), _, ["arobase_wrapper"])*/
              |> append_body(acc, _)

          /*| {Newline} -> flush_para(acc, [])*/
          | {Newline} -> flush_line(acc)
          | ~{Code} -> append(acc, <span class="code">{Code}</span>)

          | { Format = (s, t) } ->
            x = flush_id(aux(t)).f2
            append(acc,
              match s with
              | {paragraph} -> <>{x}</>
              | {title = i} ->
                (match i with
                | 1 -> <h1>{x}</h1>
                | 2 -> <h2>{x}</h2>
                | 3 -> <h3>{x}</h3>
                | 4 -> <h4>{x}</h4>
                | 5 -> <h5>{x}</h5>
                | 6 -> <h6>{x}</h6>
                | _ -> <span class="title">{String.of_int(i)} {x}</span>)
              | {bold} -> <b>{x}</b>
              | {italic} -> <i>{x}</i>
              | {link = l} -> <a href="{l}">{x}</a>
              | {link_elt} -> (
                // {! ModulePath.function}
                match t.l with
                | [ { Raw = path } ] -> with_link(String.trim(path), <>{x}</>)
                | _ -> x // else, we ignore the format
              )
              | {emphasize}   -> <em>{x}</em>
              | {superscript} -> <sup>{x}</sup>
              | {subscript}   -> <sub>{x}</sub>
              | {center}      -> <p class="text-center">{x}</p>
              | {left}        -> <p class="text-left">{x}</p>
              | {right}       -> <p class="text-right">{x}</p>
              | {latex}       -> <span class="latex">{x}</span>
              | {code_pre}    -> <pre class="code_pre">{x}</pre>
              | {verbatim}    -> <pre class="verbatim">{x}</pre>
              | {custom}      -> <span class="custom">&nbsp;{x}&nbsp;</span>
              | {list}        -> <span class="list">list</span><span>{x}</span>
              | {enum}        -> <span class="enum">enum</span><span>{x}</span>)
        ), (<></>, <></>), l)
  /*(_, block_res) = close_para(aux(res), [])*/
  (_, block_res) = flush_id(aux(res))
  <div class="block">{ block_res }</div>

  print_comment(hyperlink:Join.Html.link.map, {~content ; ... } : Comment.comment ) : xhtml =
    to_xhtml(hyperlink, content, {Doc})

}}
