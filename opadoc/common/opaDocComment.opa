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


OpaDocComment =

  private =
  {{
    Raw_comment =
    {{

      escape_code(txt: string): string =
        replacements = [("\{", "\\\{"), ("\}", "\\\}"), ("[", "\\["), ("]", "\\]")]
        List.fold(((s, r), f -> f @ String.replace(s, r, _)), replacements, identity)(txt)

      f_concat(f, l) =
        mk_b(xs, l) = match l with
          { nil } -> xs
          l -> { Raw = Text.to_string(f(List.rev(l))) } +> xs

        rec aux(lacc, l : list(OpaDocTy.txt_element)) = match l with
          { nil } -> mk_b([], lacc)
          ~{ hd tl } ->
          m(x) = mk_b((x +> aux([], tl)), lacc)
          ml({l = l}) = { l = aux([], l) }
          (match hd with
            { Raw = b } -> aux(b +> lacc, tl)

            ~{ License } ->  m(~{ License } : OpaDocTy.txt_element)
            ~{ Authors } ->  m(~{ Authors } : OpaDocTy.txt_element)
            { Param = (p, l) } -> m({ Param = (p, ml({l = l}).l) } : OpaDocTy.txt_element)
            { Return = l } -> m({ Return = ml({l = l}).l } : OpaDocTy.txt_element)
            { Other_arobase = (n, ll, l) } -> m({ Other_arobase = (n, ll, ml({l = l}).l) } : OpaDocTy.txt_element)

            { Format = (s, l) } -> m({ Format = (s, ml(l)) } : OpaDocTy.txt_element)
            ~{ Code } -> m(~{ Code } : OpaDocTy.txt_element)
            { Newline } -> m({ Newline } : OpaDocTy.txt_element)
            { Esc_accol } -> m({ Esc_accol } : OpaDocTy.txt_element)
            { Esc_croch } -> m({ Esc_croch } : OpaDocTy.txt_element)
          )
        aux([], l)

      p_list(elem : Parser.general_parser(list(OpaDocTy.format2)),
             sep : Parser.general_parser(OpaDocTy.txt_element)) = parser
        | x=elem xs=(s=sep v=elem -> (s, v))* -> (x, xs)
      // ancienne forme qui ne marche pas : reg = parser ((![{])*) -> Text.to_string(__1)

      accol_beg = parser "\{" Rule.ws -> void
      accol_end = parser Rule.ws t=((!"\}" .)*) "\}" -> Text.to_string(t)

      crochet_rec = parser r=(Rule.ws "[" crochet_rec* ((!"]" .)*) "]") -> r
      crochet_beg = parser "[" Rule.ws -> void
      crochet_end = parser  Rule.ws t=((crochet_rec*) ((!"]" .)*)) "]" -> Text.to_string(t)

      /*
      newline = parser "\n" -> void
      abbrev_plus_beg = parser newline Rule.ws "+" Rule.ws -> void
      abbrev_plus_end = parser Rule.ws .* -> void
      */

      elem_accol_croch = parser r=( "\n" "\n"+ -> {newp} | "\n" -> {newl} |  r=("\\\{" | "\\[" | ![{[] .) -> {str = r}  )* -> r

      accol_croch_rec = parser
        | "\\\{" -> { Esc_accol } : OpaDocTy.txt_element
        | "\\[" -> { Esc_croch }
        | "\n" -> { Newline }
        | c=. -> { Raw = c }

      accol_rec_beg = parser t=("\{") -> { Raw = Text.get(t, 0) }
      accol_rec_end = parser t=("}") -> { Raw = Text.get(t, 0) }
      accol_rec_ws = parser r=(t=Rule.white_space -> { Raw = Text.get(t, 0) })* -> r
      accol_rec = parser __1=accol_rec_ws __2=accol_rec_beg __3=accol_rec* __4=(!"}" __2=accol_croch_rec -> __2)* __5=accol_rec_end -> __1 ++ [__2] ++ List.flatten(__3) ++ __4 ++ [__5]

      accol_star = parser Rule.ws __2=(__1=accol_rec* __2=(!"}" __2=accol_croch_rec -> __2)* -> List.flatten(__1) ++ __2) "}" -> { l = __2 }

      /* Text formatting: {<format> <text>} or [<code>] */
      accol_croch = parser
        | accol_beg
          __2=(__1=( "ul" -> {list}
          | "enum" -> {enum}
          | nat=Rule.natural -> {title = nat}
          | "b" -> {bold}
          | "it" -> {italic}
          | accol_beg ":" __3=accol_end -> {link = __3}
          | "!" -> {link_elt}
          | "emp" -> {emphasize}
          | "^" -> {superscript}
          | "_" -> {subscript}
          | "C" -> {center}
          | "L" -> {left}
          | "R" -> {right}
          | "%" -> {latex}
          | "[" -> {code_pre}
          | "v" -> {verbatim}
          ) __2=accol_star -> (__1, __2)
          | __1=accol_star -> ({custom}, __1)) -> {Format = __2}

        | crochet_beg __2=crochet_end -> {Code = __2}

      main2 = p_list(elem_accol_croch, accol_croch)

      parse(s) =
        `++` = List.append
        parse_comment(s) =
          match Parser.try_parse(main2, s) with
            {none} ->
              do warning("Failure : comment parsing normal, raw string returned")
              [ { Raw = s } ]
            ~{some} ->
              close_para(acc, para) = ({Format = ({paragraph}, {l = [{Raw = para}]})} +> acc, Text.cons(""))
              rec conv_para(l, (acc, para)) = match l with
                | [] -> (acc, para)
                | [hd | tl] -> (match hd with
                  | {newp} -> conv_para(tl, close_para(acc, para))
                  | {newl} -> conv_para(tl, (acc, Text.concat(Text.cons("\n"), para)))
                  | ~{str} -> conv_para(tl, (acc, Text.concat(str, para))))
              and conv_aux(l, (acc, para)) = match l with
                | [] -> (acc, para)
                | [hd | tl] -> (match hd with
                  | {newp} -> conv_para(tl, (acc, para))
                  | {newl} -> conv_aux(tl, (acc, Text.concat(Text.cons("\n"), para)))
                  | ~{str} -> conv_aux(tl, (acc, Text.concat(str, para))))

              aux(l) =
                (acc, para) = conv_aux(List.rev(l), ([], Text.cons("")))
                texts = {Raw = para} +> acc
                f_concat(Text.ltconcat, texts)
              conv(conv_acc, (abr, s)) =
                conv_res = aux(s)
                f_concat(Text.lcconcat, [abr]) ++ (conv_res ++ conv_acc)
              /*conv([], ({nil}, some.f1)) ++ List.fold_right(conv, some.f2, [])*/
              aux(some.f1) ++ List.fold_right(conv, some.f2, [])

        (s, l) = split_multi_arobase(s)

        param = parser "@param" Rule.ws i=Rule.ident Rule.ws com=(.*) -> { Param = (i, parse_comment(Text.to_string(com))) }
        return = parser "@return" com=(.*) -> { Return = parse_comment(Text.to_string(com)) }

        author = parser "@" "a"? "uthor" "s"? (" " | ":")* com=(.*) -> { Authors = [ Text.to_string(com) ] }
        category = parser "@category" Rule.ws i=Rule.ident Rule.ws com=(.*) -> { Other_arobase = ("category", [i], parse_comment(Text.to_string(com))) }
        destination = parser "@destination" Rule.ws i=Rule.ident Rule.ws com=(.*) -> { Other_arobase = ("destination", [i], parse_comment(Text.to_string(com))) }
        stability = parser "@stability" Rule.ws i=Rule.ident Rule.ws com=(.*) -> { Other_arobase = ("stability", [i], parse_comment(Text.to_string(com))) }
        unknown_arobase = parser "@" i=Rule.ident Rule.ws com=(.*) -> { Other_arobase = (i, [], parse_comment(Text.to_string(com))) }

        insert_nl(l) =
          rec aux(l) = match l with
            | [] -> []
            | [x|[]] -> [x]
            | [x|xs] -> x +> ({Newline} +> aux(xs))
          aux(l)

        { l = parse_comment(s) ++ ({Newline} +> insert_nl(List.map(Parser.parse(parser
            | Rule.ws r=(r=param -> r | r=return -> r | r=author -> r | r=category -> r | r=destination -> r | r=stability -> r | r=unknown_arobase -> r) -> r
            | s=(.*) -> s = Text.to_string(s) do jlog("aaaaaaaaaaaaaaaaaaaa\n{s}")
               //do warning("Failure : comment parsing aro, raw string returned")
              {Raw = s}
            , _), l)))}


      split_newline = Parser.parse(Rule.parse_list(parser t=(!"\n" .)* -> Text.to_string(Text.ltconcat(t)), parser "\n" -> void), _)

      split_multi_arobase(s) =
        assemble_head(l) =
          match List.index_p((x -> match x with {aro = _} -> true | {normal = _} -> false), l) with
            { none } -> (l, [])
            { some = i } -> List.split_at(l, i)

        f = x -> match x with {aro = _} -> error("assert false") {normal = s} -> Text.to_string(s)

        to_s = List.to_string_using("", "", "\n", _)

        rec aux(l) =
          match l with
            [] -> []
            [x|xs] ->
              (l1, l2) = assemble_head(xs)
               to_s((match x with {normal = _} -> error("assert false") {aro = s} -> Text.to_string(s)) +> List.map(f, l1)) +> aux(l2)

        (l1, l2) =
          assemble_head(List.map(Parser.parse(parser d=(Rule.ws "@" Rule.alphanum_char Rule.ws .*) -> {aro = d} | d=(.*) -> {normal = d}, _), split_newline(s)))

        (to_s(List.map(f, l1)), aux(l2))

      delete_star(s) = /* si chaque ligne débute par (des blancs, une étoile et des blancs), cette fonction les supprime, rien sinon */
        match
          List.fold_left((acc, s ->
              match acc with
                { some = l } ->
                  (match Parser.try_parse(parser Rule.ws "*" Rule.ws rest=.* -> rest, s) with
                    {none} -> {none}
                    {some = s} -> { some = s +> l })
                {none} -> {none}
            )
            , {some = []}
            , split_newline(Text.to_string(Text.ltconcat(Parser.parse(parser Rule.ws __2=(!(Rule.ws Rule.eos) .)* Rule.ws -> __2, s)))))
        with
          {none} -> s
          {some = l} -> List.to_string_using("", "", "\n", List.map((s -> Text.to_string(Text.lcconcat(s))), List.rev(l)))

      authors_opt = parser d=("@" ("a"? "uthor" "s"?)? (" " | ":")* n=("?" -> {none} | __1=(!"\n" .)* -> Parser.parse(parser Rule.ws -> {none} | rest=.* -> {some = rest}, Text.to_string(Text.ltconcat(__1)))) Rule.ws -> n)* ->
                      List.rev(List.fold_left((acc, o -> match o with
                         | {none} -> acc
                         | {some = s} -> Text.to_string(Text.lcconcat(s)) +> acc), [], d))
    }}

    string = parser ("\"" (!"\"" .)* "\"") -> void

    garbage = parser (string -> void | (!doc_comment .) -> void)* -> void

    /**
     * Returns the position without consuming any chararacter
    **/
    pos = parser p={(_, (it: itextrator)-> some((it, Itextrator.pos(it)))):Parser.general_parser(int)} -> p:int

    inside_doc_comment = parser r=(string+ | doc_comment+ | !"*/" . )* -> Text.ltconcat(r)

    sp = parser [\t ]* -> void

    rt = parser [\n ] -> void

    empty_line = parser sp rt sp rt -> void

    doc_comment = parser
        | "/**" ~inside_doc_comment "*/" ~pos empty_line ->
           ({Glob}, pos, Raw_comment.delete_star(Text.to_string(inside_doc_comment)))
        | "/**" ~inside_doc_comment "*/" ~pos ->
           ({Assoc}, pos, Raw_comment.delete_star(Text.to_string(inside_doc_comment)))

    package = parser "package" Rule.ws r=(([a-zA-Z0-9_.\-] .)+) -> Text.to_string(r)

    extract = parser comments=(garbage ~doc_comment -> doc_comment)* garbage -> comments

    extension = "doc_comment"
  }}
  public = /*open private*/
  {{

    lp = parser Rule.ws
           "--" Rule.ws "begin" Rule.ws "LICENCE" Rule.ws
           "(c)" Rule.ws d_beg=Rule.natural d_end_opt=(Rule.ws ([\-,] .) Rule.ws d_end=Rule.natural -> d_end)? (Rule.ws "," Rule.ws Rule.natural)? Rule.ws "MLstate" Rule.ws
           "All rights reserved." Rule.ws
           "This file is confidential and intended solely for the addressee(s)." Rule.ws
           "Any unauthorized use or dissemination is prohibited." Rule.ws
           "end" Rule.ws "LICENCE"? Rule.ws "--"? Rule.ws
           l=private.Raw_comment.authors_opt
           "*"? Rule.ws -> (d_beg, d_end_opt, l)

    from_opa_file(fname)=
      file_content = %% BslFile.content %% : string -> string
      content = file_content(fname)

      //do jlog(fname)
      fname = OpaDocUtils.relative_path(fname)

      match Parser.parse(private.extract, content) with
      | [] -> []
      | [(cat, pos, c) | l] ->
        (o, l) = // TODO: parse multiple end dates
          match Parser.try_parse(lp, c) with
           {none} -> _ = Parser.parse(b = parser "begin LICENCE" -> void parser (!b .)* b .* -> do jlog(c) void | .* -> void, c) ({none}, [(cat, pos, c) | l])
           {some = (i1, i2_opt, l_auth)} ->
             ({some = {~fname ~pos ~cat content = {l =
               [ {License = (i1, i2_opt)}
               , {Authors = l_auth } ]} }}, l)
        l = List.map(
              (cat, pos, content) -> { ~fname ~pos ~cat content = private.Raw_comment.parse(content) }
            , l)
        match o with
        {none} -> l
        {some = s} -> s +> l

    //to_file(fname,l) = JsonFile.to_file("{fname}.{private.extension}",l)

  }}
  public
