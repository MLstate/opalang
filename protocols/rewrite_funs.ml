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
  FIXME:
  1) remove open
  2) use ocaml ast instead of verbatim
     NO.  There are still too many missing features.
     -- module RRequest = Qos.Ressource(
     -- for ... do etc...
*)
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module Cons = O.Cons
module G = Grammar
module T = Tools
module L = B.List

exception NoStateDefined

(*let str_of_expr te = let b = Buffer.create 1024 in (OcamlPrint.Buf.expr b te; Buffer.contents b)*)

(* Note: the <CR> is essential because ppdebug.pl is line-based. *)
let ifstr = function
  | Some level -> B.sprintf "#<If$minlevel %d>" level
  | None -> "#<If>"

let os s_opt = Option.default "" s_opt

let wrapexnverb exnwrap exnname verb =
  if exnwrap
  then Printf.sprintf "(try (%s) with exn -> _err_cont (exn,%s,if _runtime.rt_proto.rt_backtrace then (Some (Printexc.get_backtrace())) else None) _runtime sched conn _mailbox _err_cont)" verb (os exnname)
  else verb

let verbstr (dbg,level,exnwrap,exnname,verb) =
  let verb = wrapexnverb exnwrap exnname verb in
  match dbg with
  | Some "debug" -> (ifstr level)^verb^"#<End>\n"
  | Some "release" -> (ifstr level)^"()#<Else>()"^verb^"\n#<End>\n"
  | _ -> verb

let wrapexnexpr exnwrap exnname expr =
  if exnwrap
  then O.Try (expr,[(O.PatVar(Ident.source "exn"),None,
                     O.make_AppL ([O.make_Var "_err_cont";
                                   (O.Tuple [O.make_Var "exn";
                                             Cons.string (os exnname);
                                             O.Verbatim "(if _runtime.rt_proto.rt_backtrace then (Some (Printexc.get_backtrace())) else None)";
                                            ]);
                                   O.make_Var "_runtime"; O.make_Var "sched";
                                   O.make_Var "conn"; O.make_Var "_mailbox"; ]))])
  else expr

let dbgexpr (dbg,level,exnwrap,exnname,expr) =
  let expr = wrapexnexpr exnwrap exnname expr in
  match dbg with
  | Some "debug" -> Cons.sequence [O.Verbatim ((ifstr level)^"()"); expr; O.Verbatim "#<End>\n"]
  | Some "release" -> Cons.sequence [O.Verbatim ((ifstr level)^"()#<Else>()"); expr; O.Verbatim "#<End>\n"]
  | _ -> expr

let make_pv x = O.PatVar (Ident.source x)

let make_App app = function
  | G.Ocaml (dbg,l,ew,en,e) -> O.App (app, (dbgexpr (dbg,l,ew,en,e)))
  | G.GVerbatim (dbg,l,ew,en,s) -> O.App (app, O.Verbatim (verbstr (dbg,l,ew,en,s)))
  | _ -> assert false

let make_fp = function
  | G.GVar (str, _type) -> O.Pat (O.PatVar (Ident.source str))
  | _ -> assert false

let rec make_pat = function
  | G.Underscore -> O.PatAny
  | G.Ident s -> O.PatVar (Ident.source s)
  | G.Constr (name, params) ->
      O.PatConstructor (
        [Ident.source name] ,
        L.map (make_pv @* fst) <| L.map T.tuple_of_var params
      )
  | _ -> assert false

and rewrite_clause = function
  | G.Case ((constr, guard), code) ->
      let pattern = make_pat constr in
      let guard = Option.map (
        function
        | (G.Ocaml (dbg,l,ew,en,x)) -> dbgexpr (dbg,l,ew,en,x)
        | (G.GVerbatim (dbg,l,ew,en,x)) -> O.Verbatim (verbstr (dbg,l,ew,en,x))
        | _ -> assert false
      ) guard in
      pattern, guard, rewrite_code code
  | _ -> assert false

and receive rfn success errors timeout =

  let make_err_cont clauses timeout =
    let clauses = match timeout with
      | None -> clauses
      | Some (G.Timeout ( _, what_to_do)) ->
          let clause = G.Case ((G.Constr ("Timeout", []), None), what_to_do) in
            clause :: clauses
      | _ -> assert false in
    let clauses =
      match L.map rewrite_clause clauses with
      | [] -> [O.PatAny, None, O.Const O.Unit]
      | [((O.PatConstructor ([timeout], _) as patconst), g, re)] when Ident.stident timeout = "Timeout" ->
          (patconst, g, re) :: [O.PatAny, None, O.Const O.Unit]
      | otherwise -> otherwise
    in O.Var (
        O.Labeled (
          "err_cont" ,
          Some ( O.Function ( clauses ) )
        )
      )
  in

  let get_timeout = function
    | None -> []
    | Some (G.Timeout (float, _)) ->
        [O.Var (O.Labeled ("timeout", Some (O.Verbatim float)))]
    | _ -> assert false in

  let apps = O.Verbatim rfn
    :: make_err_cont errors timeout
    :: get_timeout timeout
    @[ success ]
  in

  O.make_AppL apps

and make_recv rfn v vstr clst elst timeout tail =
  let success = O.Abs ([O.pf "v"], O.Match (O.make_Var "v", L.map rewrite_clause clst)) in
  let recv = receive rfn success elst timeout in
  O.Letin ([(O.pf vstr, v)],
         (match tail with
          | [] -> recv
          | lst -> O.Sequence (recv, rewrite_code lst)))

and rewrite_code = function
  | [] -> O.Const O.Unit

  | G.Block lst::tail -> O.Sequence (rewrite_code lst, rewrite_code tail)

  | G.Close::lst ->
      (*let close = O.Verbatim "Scheduler.remove_connection sched conn" in*)
      let close = O.Verbatim "close_conn sched conn _mailbox" in
      (match lst with [] -> close | _ -> O.Sequence (close, rewrite_code lst))

  | G.Call (n,params)::lst ->
      if (not <| L.is_empty lst) then
        raise <| T.Operation_after_statecall n
      else L.fold_left make_App (O.Verbatim ("proto_" ^ n)) (params @ [G.GVerbatim (None,None,false,None,"_runtime");
                                                                       G.GVerbatim (None,None,false,None,"sched");
                                                                       G.GVerbatim (None,None,false,None,"conn");
                                                                       G.GVerbatim (None,None,false,None,"_mailbox");
                                                                       G.GVerbatim (None,None,false,None,"_err_cont");
                                                                      ])

  | G.GLet (var, G.Async (exnwrap,exnname,expr))::tail ->
      O.App (
        O.Abs([O.pf "__proto_temp_cont"],
              wrapexnexpr exnwrap exnname (O.App (expr,O.make_Var "__proto_temp_cont"))),
              O.Abs ([O.Pat (O.PatVar (Ident.source (fst <| T.tuple_of_var var)))],
                     rewrite_code tail))

  | G.GLet (var, G.Ocaml (dbg,level,exnwrap,exnname,value))::tail ->
      wrapexnexpr exnwrap exnname (O.Letin ([make_fp var, dbgexpr (dbg,level,false,None,value)], rewrite_code tail))

  | G.GLet (var, G.GVerbatim (dbg,level,exnwrap,exnname,value))::tail ->
      wrapexnexpr exnwrap exnname (O.Letin ([make_fp var, O.Verbatim (verbstr (dbg,level,false,None,value))], rewrite_code tail))

  | G.Errcont fn::tail ->
      O.Letin ([O.pf "_err_cont", O.App (O.make_Var "ecsa2ec",O.make_Var ("proto_"^fn))], rewrite_code tail)

  | G.Ocaml (dbg,l,ew,en,e)::tail -> (match tail with
    | [] -> dbgexpr (dbg,l,ew,en,e)
    | _ -> wrapexnexpr ew en (O.Sequence (dbgexpr (dbg,l,false,None,e), rewrite_code tail)))

  (*| G.OcamlTop sl::tail -> NO!!!
      Cons.sequence (sl@(match tail with | [] -> [] | _ -> [rewrite_code tail]))*)

  | G.GVerbatim (dbg,l,ew,en,s)::tail -> (match tail with
    | [] -> O.Verbatim (verbstr (dbg,l,ew,en,s))
    | _ -> wrapexnexpr ew en (O.Sequence (O.Verbatim (verbstr (dbg,l,false,None,s)), rewrite_code tail)))

  | G.If (G.Ocaml (_,_,_,_,c), G.Block t, G.Block e)::tail ->
      let cond = O.Cond (c, rewrite_code t, rewrite_code e)
      in (match tail with [] -> cond | _ -> O.Sequence (cond, rewrite_code tail))

  | G.If (G.GVerbatim (_,_,_,_,c), G.Block t, G.Block e)::tail ->
      let cond = O.Cond (O.Verbatim c, rewrite_code t, rewrite_code e)
      in (match tail with [] -> cond | _ -> O.Sequence (cond, rewrite_code tail))

  | G.GMatch (ew, en, e, pelst)::tail ->
      (* TODO: implement guards *)
      let mtch = O.Match (e, L.map (function ((g,p),G.Block e) -> (p,g,rewrite_code e) | _ -> assert false) pelst) in
      wrapexnexpr ew en
        (match tail with
         | [] -> mtch
         | _ -> O.Sequence (mtch, rewrite_code tail))

  | G.Send msg::tail ->
      O.App (
        O.App (
          (O.Verbatim "write_errcont _runtime sched conn"),
          (O.App (O.Verbatim "string_of_msg", O.Verbatim msg))),
        O.Abs ([O.Pat O.PatAny], rewrite_code tail)
        )

  | G.SendBuf buf::tail ->
      O.App (
        O.App (
          (O.Verbatim "write_errcont _runtime sched conn"), buf),
        O.Abs ([O.Pat O.PatAny], rewrite_code tail)
        )

  | G.Listen (var, port_spec_exp, fn)::tail ->
      let fnstr = String.concat " " fn in
      let l = O.Letin ([(O.Pat (O.PatTuple [O.PatVar (Ident.source "port_spec"); O.PatVar (Ident.source "sec_mode")]), port_spec_exp)],
                     O.Verbatim ("Network.listen sched port_spec sec_mode (fun _ conn2 -> proto_"
                               ^fnstr^" _runtime sched conn2 (new_mailbox _runtime) _err_cont)")) in
      O.Letin ([(make_fp var),l],rewrite_code tail)

  | G.ReadConn (var,conn_blk)::tail ->
      O.Letin ([(O.Pat (O.PatTuple [O.PatVar (Ident.source "conn2"); O.PatVar (Ident.source "_blksize")]), O.Verbatim conn_blk)],
             (O.App (O.Verbatim "Scheduler.read sched conn2 ",
                   O.Abs ([make_fp var], rewrite_code tail))))

  | G.WriteConn conn_msg::tail ->
      O.App (
        O.Letin ([(O.Pat (O.PatTuple [O.PatVar (Ident.source "conn2"); O.PatVar (Ident.source "msg")]),
                 O.Verbatim conn_msg)],
               O.Verbatim "write_errcont _runtime sched conn2 msg"),
        O.Abs ([O.Pat O.PatAny], rewrite_code tail)
      )

  (*| G.Sleep time::tail ->
      O.App (O.Letin ([(O.Pat (O.PatVar (Ident.source "time")), time)],
                      O.Verbatim "(fun cont -> ignore (Scheduler.sleep sched time cont))"),
             O.Abs ([O.Pat O.PatAny], rewrite_code tail))*)

  | G.Connect (port_spec_exp, fn)::tail ->
      let fnstr = String.concat " " fn in
      let l =
        O.Letin ([(O.Pat (O.PatTuple [O.PatVar (Ident.source "port_spec"); O.PatVar (Ident.source "sec_mode")]), port_spec_exp)],
               O.Verbatim ("Network.connect sched port_spec sec_mode (fun conn2 -> proto_"
                         ^fnstr^" conn2 _runtime sched conn _mailbox _err_cont)")) in
      (match tail with
       | [] -> l
       | _ -> O.Sequence (l, rewrite_code tail))

  | G.SendAll lst::tail ->
      O.Sequence (
        O.App ( O.App (
            O.Verbatim "List.iter",
            O.Abs ([O.Pat (O.PatVar (Ident.source "msg"))],
              O.App ( O.App (
                  O.Verbatim "write_errcont _runtime sched conn",
                  O.App (O.Verbatim "string_of_msg", O.Verbatim "msg")),
                O.Abs ([O.Pat O.PatAny], O.Const O.Unit)))),
          O.Verbatim lst),
        rewrite_code tail)

  | G.Receive (_, _, clst, elst, timeout)::tail ->
      let rfn = "receive _runtime sched conn _mailbox" in
      let success =
        O.Function (L.map rewrite_clause clst)
(*
        O.Abs (
          [O.pf "msg"],
          (*[O.Pat (O.PatConst O.Unit)] ,*)
          O.Match (
            (*O.Verbatim "Queue.pop (getmb _mailbox)" ,*)
            O.make_Var "msg",
            L.map rewrite_clause clst
          )
        )
*)
      in
      let receive = receive rfn success elst timeout in
      (match tail with
       | [] -> receive
       | lst -> O.Sequence (receive, rewrite_code lst))

  | G.Upto (mark, clst, elst, timeout)::tail ->
      (*let rfn = "HttpTools.read_upto_stream_cps ~inclusive:false _runtime.rt_buf (getmbox conn _mailbox) mark sched" in*)
      let rfn = "HttpTools.upto_mark_stream_cps3 ~inclusive:false sched conn _mailbox mark ?callback:_runtime.rt_tmp.rt_callback _runtime.rt_proto.rt_payload ~blocksize:_runtime.rt_proto.rt_block_size" in
      make_recv rfn mark "mark" clst elst timeout tail

  | G.Fixed (cnt, clst, elst, timeout)::tail ->
      (*let rfn = "HttpTools.read_fixed_stream_cps _runtime.rt_buf (getmbox conn _mailbox) cnt sched" in*)
      let rfn = "HttpTools.fixed_stream_cps3 sched conn _mailbox cnt ?callback:_runtime.rt_tmp.rt_callback _runtime.rt_proto.rt_payload ~blocksize:_runtime.rt_proto.rt_block_size" in
      make_recv rfn cnt "cnt" clst elst timeout tail

  | G.Content (content, clst, elst, timeout)::tail ->
      let rfn = "Scheduler.read_content sched conn content " in
      make_recv rfn content "content" clst elst timeout tail

  | G.ReadRaw (clst, elst, timeout)::tail ->
      let rfn = "Scheduler.read sched conn " in
      make_recv rfn Cons.unit "()" clst elst timeout tail

  | G.Sleep time_expr::tail ->
      O.App (O.Letin ([(O.pf "time" , time_expr)],
                      O.Verbatim ("(fun c -> ignore (Scheduler.sleep sched time c))")),
             O.Abs ([O.Pat O.PatAny], rewrite_code tail))

  |_ -> assert false


let rewrite_fun f =
  let make_fun (n, params, b) =
    try
      O.Pat (O.PatVar (Ident.source ("proto_" ^ n))),
      O.Abs (
        (L.map make_fp params) @ [O.Pat (O.PatVar (Ident.source " _runtime"));
                                  O.Pat (O.PatVar (Ident.source " sched"));
                                  O.Pat (O.PatVar (Ident.source " conn"));
                                  O.Pat (O.PatVar (Ident.source " _mailbox"));
                                  O.Pat (O.PatVar (Ident.source " _err_cont"));
                                 ],
        rewrite_code b
      )
    with
      | T.Operation_after_statecall s ->
          B.sprintf "Error in state ' %s ' : no operation can be done after going to state : %s" n s
          |> failwith
      | e -> raise e
  in match f with
  | Reorder_functions.Rec (lst) -> O.Letrec (L.map make_fun lst)
  | Reorder_functions.Normal (n,p,b) -> O.Let ([make_fun (n,p,b)])

(* Generating functions *)
let gen_funs = function
  | [] -> raise NoStateDefined
  | lst -> L.rev_map rewrite_fun <| Reorder_functions.do_it lst
