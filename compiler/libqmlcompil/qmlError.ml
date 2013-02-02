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
(* refactoring *)

(* alias *)
module Q = QmlAst

module Context =
struct

  type context = {
    pos : FilePos.pos ;

    (* The annotmap is for finding types *)
    annotmap : QmlAst.annotmap list ;

    code_elt : QmlAst.code_elt list ;

    expr : QmlAst.expr list ;

    pat : QmlAst.pat list ;

    ty : QmlAst.ty list ;

    package : string list ; (* sometime the context is related to compilation option of a package, so you are not on shame but you have no position *)

    what_a_shame : string list ;
  }

  let default = {
    pos = FilePos.nopos "QmlError.Context.default" ;
    annotmap = [] ;
    code_elt = [] ;
    expr = [] ;
    pat = [] ;
    ty = [] ;
    package = [] ;
    what_a_shame = [] ;
  }

  let insert_aux list o =
    if List.exists (fun o' -> o == o') list
    then list
    else o::list

  let merge_aux a b = List.fold_left insert_aux b a

  let merge2 c1 c2 = {
    pos = FilePos.merge_pos c1.pos c2.pos ;

    annotmap = merge_aux c1.annotmap c2.annotmap ;
    code_elt = merge_aux c1.code_elt c2.code_elt ;
    expr = merge_aux c1.expr c2.expr ;
    pat = merge_aux c1.pat c2.pat ;
    ty = merge_aux c1.ty c2.ty ;
    package = merge_aux c1.package c2.package;
    what_a_shame = merge_aux c1.what_a_shame c2.what_a_shame ;
  }
  let merge = List.fold_left merge2

  let pos pos = { default with pos = pos }
  let label label = { default with pos = Annot.pos label }
  let annotmap annotmap = { default with annotmap = [annotmap] }
  let code_elt code_elt = { default with code_elt = [code_elt] ; pos = QmlAst.Pos.code_elt code_elt }
  let expr expr = { default with expr = [expr] }
  let exprs expr exprs = { default with expr = expr::exprs }
  let pat pat = { default with pat = [pat] }
  let ty ty = { default with ty = [ty] }
  let package packagename = { default with package = [packagename] }
  let shame_on_me_i_am_too_lazy no_context = { default with what_a_shame = [no_context] }


  let annoted_expr annotmap expr = { default with annotmap = [annotmap] ; expr = [expr] }
  let annoted_pat annotmap pat = { default with annotmap = [annotmap] ; pat = [pat] }

  (* OUTPUT *)

  (*
    The goal there:
    We try at least to print the location in the console.
    If we are not able to do that, we will print some AST, using [QmlPrint.pp].
    The full printer is used internally for opatrack traces, as the code
    has been transormed, we print totally
  *)

  module Output =
  struct
    (* merge all positions found from the context *)
    let extract_position c =
      let fold pos acc e =
        FilePos.merge_pos acc (pos e)
      in
      let pos = c.pos in
      let pos = List.fold_left (fold Q.Pos.expr) pos c.expr in
      let pos = List.fold_left (fold Q.Pos.pat) pos c.pat in
      pos

    (* strategy:
       + if we have some positions, we print just them, it is enough,
         printing the code is not a good idea because it does not correspond to
         what the user wrote anyway.
       + if we do not have positions, it is because we are not finished with the refactoring
         of positions in the AST. In this case, will will print the 'full' printer in the console.
    *)

    let sep = String.make 80 '='

    let of_type annot fmt c =
      let iter annotmap =
        match QmlAnnotMap.find_ty_opt annot annotmap with
        | Some ty ->
            Format.fprintf fmt "%s@\nAnnoted with the following type:@\n%a@\n"
              sep QmlPrint.pp#ty ty
        | None -> () in
      List.iter iter c.annotmap

    let full fmt c =
      let pos = extract_position c in

      Format.fprintf fmt "%s@\n%a@\n" sep FilePos.pp_pos pos ;

      if c.expr = [] && c.pat = [] then (

      List.iter (fun code_elt ->
                   Format.fprintf fmt "%s@\nIn the following toplevel definition:@\n%a@\n"
                     sep QmlPrint.pp#code_elt code_elt) c.code_elt ;

        ()

      );

      List.iter (fun expr ->
                   Format.fprintf fmt "%s@\nIn the following expression:@\n%a@\n%a"
                     sep QmlPrint.pp#expr expr (of_type (Q.QAnnot.expr expr)) c) c.expr ;

      List.iter (fun pat ->
                   Format.fprintf fmt "%s@\nIn the following pattern:@\n%a@\n%a"
                     sep QmlPrint.pp#pat pat (of_type (Q.QAnnot.pat pat)) c) c.pat ;

      List.iter (fun ty ->
                   Format.fprintf fmt "%s@\nIn the following type:@\n%a@\n"
                     sep QmlPrint.pp#ty ty) c.ty ;

      List.iter (fun package ->
                   Format.fprintf fmt "%s@\nIn the following package:@\n%a@\n"
                     sep Format.pp_print_string package) c.package ;
      ();

      List.iter (fun shame ->
                   Format.fprintf fmt "%s@\nIn the following internal positions:@\n%a@\n"
                     sep Format.pp_print_string shame) c.what_a_shame ;
      ()

    let console fmt c =
      let pos = extract_position c in
      if FilePos.is_empty pos
      then full fmt c
      else Format.fprintf fmt "%a@\n" FilePos.pp_pos pos
  end

  let full = Output.full
  let console = Output.console
  let get_pos = Output.extract_position

end
type context = Context.context
module E = PassError.LangError(Context)
include E
