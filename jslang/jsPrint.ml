(*
    Copyright © 2011, 2012 MLstate

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
(* CF mli *)

(* depends *)
let (|>) = InfixOperator.(|>)
module String = BaseString
module Format = BaseFormat

(* shorthands *)
module J = JsAst
module Q = QmlAst

(* -- *)

let std_val_regexp = Str.regexp "^[a-zA-Z_$][a-zA-Z0-9_$]*$"
let is_std_val s =
  (not (J.is_keyword s)) &&
    (Str.string_match std_val_regexp s 0)

let escape_string_regexp = Str.regexp "[\"\\\000-\031]"
let escape_string ?(double_quote=true) s =
  (* we can't rely on ocaml's escaping since it transforms accents to special syntax like \195
     which javascript do not handle. We prefer to keep accents in UTF-8 *)
  let escape_matched whole =
    let s = Str.matched_string whole in
    "\\" ^ match s with
    | "\n" -> "n"
    | "\r" -> "r"
    | c ->
        let v = (Char.code (String.get c 0)) in
        if v < 32 then Printf.sprintf "x%02x" v
        else s
  in
  let s = Str.global_substitute escape_string_regexp escape_matched s in
  if double_quote
  then
    Printf.sprintf "\"%s\"" s
  else
    Printf.sprintf "'%s'" s

let secure_field_if_needed s =
  if is_std_val s then s
  else escape_string s

let valid_chars = function
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9'
  | '_' -> true
  | _ -> false

let safe_str s = "_" ^ String.escape ~valid_chars ~escape_char:'$' s

(*
  precedence, see ECMA 262:
  http://www.ecma-international.org/publications/files/EMCA-ST/Ecma-262.pdf
*)
let p = 0
let pAssignment = 2
let pConditional = 4
let pLogicalOR = 6
let pLogicalAND = 8
let pBitwiseOR = 10
let pBitwiseXOR = 12
let pBitwiseAND = 14
let pEquality = 16
let pRelational = 18
let pShift = 20
let pAdditive = 22
let pMultiplicative = 24
let pUnary = 26
let pPostfix = 28
let pLeftHandSide = 30
let pCall = 32
let pMember = 34
let pPrimary = 36

let is_postop = function
  | J.Ju_add2_post | J.Ju_sub2_post -> true
  | _ -> false

let unop = function
  | J.Ju_delete      -> "delete"
  | J.Ju_void        -> "void"
  | J.Ju_typeof      -> "typeof"
  | J.Ju_add2_pre    -> "++"
  | J.Ju_sub2_pre    -> "--"
  | J.Ju_add_pre     -> "+"
  | J.Ju_sub_pre     -> "-"
  | J.Ju_tilde       -> "~"
  | J.Ju_not         -> "!"
  | J.Ju_add2_post   -> "++"
  | J.Ju_sub2_post   -> "--"

let binop = function
  | J.Jb_hashref     -> assert false
  | J.Jb_mul         -> "*"
  | J.Jb_div         -> "/"
  | J.Jb_mod         -> "%"
  | J.Jb_add         -> "+"
  | J.Jb_sub         -> "-"
  | J.Jb_lt          -> "<"
  | J.Jb_gt          -> ">"
  | J.Jb_leq         -> "<="
  | J.Jb_geq         -> ">="
  | J.Jb_lsr         -> ">>"
  | J.Jb_lsl         -> "<<"
  | J.Jb_asr         -> ">>>"
  | J.Jb_eq          -> "=="
  | J.Jb_neq         -> "!="
  | J.Jb_in          -> "in"
  | J.Jb_instanceof  -> "instanceof"
  | J.Jb_seq         -> "==="
  | J.Jb_sneq        -> "!=="
  | J.Jb_land        -> "&&"
  | J.Jb_lor         -> "||"
  | J.Jb_and         -> "&"
  | J.Jb_xor         -> "^"
  | J.Jb_or          -> "|"
  | J.Jb_assign      -> "="
  | J.Jb_mul_assign  -> "*="
  | J.Jb_div_assign  -> "/="
  | J.Jb_mod_assign  -> "%="
  | J.Jb_add_assign  -> "+="
  | J.Jb_sub_assign  -> "-="
  | J.Jb_lsl_assign  -> "<<="
  | J.Jb_lsr_assign  -> ">>="
  | J.Jb_asr_assign  -> ">>>="
  | J.Jb_and_assign  -> "&="
  | J.Jb_xor_assign  -> "^="
  | J.Jb_or_assign   -> "|="

let binop_prec = function
  | J.Jb_hashref     -> pCall
  | J.Jb_mul         -> pMultiplicative
  | J.Jb_div         -> pMultiplicative
  | J.Jb_mod         -> pMultiplicative
  | J.Jb_add         -> pAdditive
  | J.Jb_sub         -> pAdditive
  | J.Jb_lt          -> pRelational
  | J.Jb_gt          -> pRelational
  | J.Jb_leq         -> pRelational
  | J.Jb_geq         -> pRelational
  | J.Jb_lsr         -> pShift
  | J.Jb_lsl         -> pShift
  | J.Jb_asr         -> pShift
  | J.Jb_eq          -> pEquality
  | J.Jb_neq         -> pEquality
  | J.Jb_in          -> pRelational
  | J.Jb_instanceof  -> pRelational
  | J.Jb_seq         -> pEquality
  | J.Jb_sneq        -> pEquality
  | J.Jb_land        -> pLogicalAND
  | J.Jb_lor         -> pLogicalOR
  | J.Jb_and         -> pBitwiseAND
  | J.Jb_xor         -> pBitwiseXOR
  | J.Jb_or          -> pBitwiseOR
  | J.Jb_assign      -> pAssignment
  | J.Jb_mul_assign  -> pAssignment
  | J.Jb_div_assign  -> pAssignment
  | J.Jb_mod_assign  -> pAssignment
  | J.Jb_add_assign  -> pAssignment
  | J.Jb_sub_assign  -> pAssignment
  | J.Jb_lsl_assign  -> pAssignment
  | J.Jb_lsr_assign  -> pAssignment
  | J.Jb_asr_assign  -> pAssignment
  | J.Jb_and_assign  -> pAssignment
  | J.Jb_xor_assign  -> pAssignment
  | J.Jb_or_assign   -> pAssignment

let expr_prec = function
  | J.Je_this _      -> pPrimary
  | J.Je_ident _     -> pPrimary
  | J.Je_array _     -> pPrimary
  | J.Je_comma _     -> p
  | J.Je_object _    -> pPrimary
  | J.Je_string _    -> pPrimary
  | J.Je_num _       -> pPrimary
  | J.Je_null _      -> pPrimary
  | J.Je_undefined _ -> pPrimary
  | J.Je_bool _      -> pPrimary
  | J.Je_regexp _    -> pPrimary
  | J.Je_function _  -> pPrimary
  | J.Je_dot _       -> pCall
  | J.Je_unop (_, op, _) -> if is_postop op then pPostfix else pUnary
  | J.Je_binop (_, op, _, _) -> binop_prec op
  | J.Je_cond _      -> pConditional
  | J.Je_call _      -> pCall
  | J.Je_new _       -> pMember
  | J.Je_hole _      -> p
  | J.Je_runtime (_, JsAstRuntime.SetDistant _) -> pCall
  | J.Je_runtime (_, JsAstRuntime.TaggedString _) -> pPrimary

let pp = Format.fprintf
let pp_list = Format.pp_list
let pps = Format.pp_print_string

type 'a pprinter = Format.formatter -> 'a -> unit

class printer =
object(self)

  method ident f i =
    pps f (
      match i with
      | J.ExprIdent e -> safe_str (Ident.stident e)
      | J.Native (_, s) -> s
    )

  method unop f op = pps f (unop op)

  method field f field =
    pps f (secure_field_if_needed field)

  method objpart f (field, e) =
    pp f "@[<hv 2>%a:@ %a@]"
      self#field field
      (self#pexpr ~leading:false pAssignment) e

  method pexpr ~leading pr f e =
    if expr_prec e < pr || leading && (match e with J.Je_function _ | J.Je_object _ -> true | _ -> false)
    then pp f "(@[%a@])" (self#expr ~leading:false) e
    else (self#expr ~leading) f e

  method expr ~leading f (e : J.expr) =
    match e with
    | J.Je_this _ ->
        pps f "this"

    | J.Je_ident (_, ident) ->
        self#ident f ident

    | J.Je_array (_, list) ->
        pp f "@[<hv>[@;<1 2>%a@ ]@]"
          (pp_list ",@;<1 2>" (self#pexpr ~leading:false pAssignment)) list

    | J.Je_comma (_, [], last) ->
        self#expr ~leading f last

    | J.Je_comma (_, e :: list, last) ->
        pp f "@[%a%s%a,@;<1 2>%a@]"
          (self#pexpr ~leading pAssignment) e
          (if list = [] then "" else ",")
          (pp_list ",@;<1 2>" (self#pexpr ~leading:false pAssignment)) list
          (self#expr ~leading:false) last

    | J.Je_object (_, fields) ->
        pp f "@[<hv>{@;<1 2>%a@ }@]"
          (pp_list ",@;<1 2>" self#objpart) fields

    | J.Je_string (_, string, double_quote) ->
        pps f (escape_string ~double_quote string)

    | J.Je_num (_, num) ->
        pps f num

    | J.Je_null _ ->
        pps f "null"

    | J.Je_undefined _ ->
        pps f "undefined"

    | J.Je_bool (_, bool) ->
        if bool then pps f "true" else pps f "false"

    | J.Je_regexp (_,content,flags) ->
        (* FIXME: escaping isn't good but since parsing isn't good either and
         * they are consistent, and nobody generates regexp literals ... *)
        pp f "/%s/%s" content flags

    | J.Je_function (_, ident, params, body) ->
        pp f "@[<hv2>function %a@[<hv 1>(%a)@]{@\n%a@]@\n}"
          (Option.pp self#ident) ident
          (pp_list ",@ " self#ident) params
          self#statements body

    | J.Je_dot (_, expr, field) ->
        let field =
          if (is_std_val field) && (not (J.is_keyword field))
          then "."^field
          else "[" ^ (escape_string field) ^ "]"
        in
        pp f "@[<hv 2>%a%s@]"
          (self#pexpr ~leading pCall) expr field

    | J.Je_unop (_, op, expr) ->
        if is_postop op
        then
          pp f "@[%a%s@]"
            (self#pexpr ~leading pPostfix) expr (unop op)
        else (
          match op with
          | J.Ju_delete | J.Ju_void | J.Ju_typeof ->
              pp f "@[%s %a@]"
                (unop op) (self#pexpr ~leading:false pUnary) expr
          | _ ->
              pp f "@[%s%a@]"
                (unop op) (self#pexpr ~leading:false pUnary) expr
        )

    | J.Je_binop (_, op, expr1, expr2) -> (
        match op with
        | J.Jb_hashref ->
            pp f "@[%a[%a]@]"
              (self#pexpr ~leading pCall) expr1
              (self#pexpr ~leading:false p) expr2

        | _ ->
            let prec = binop_prec op in
            pp f "@[<hv 2>%a %s@ %a@]"
              (self#pexpr ~leading prec) expr1
              (binop op)
              (self#pexpr ~leading:false (prec + 2)) expr2
      )

    | J.Je_cond (_, cond, then_, else_) ->
        pp f "@[<hv 2>%a ?@ %a :@ %a@]"
          (self#pexpr ~leading pLogicalOR) cond
          (self#pexpr ~leading:false pAssignment) then_
          (self#pexpr ~leading:false pAssignment) else_

    | J.Je_call (_, fun_, args, _) ->
        pp f "@[%a@[<hov 1>(%a)@]@]"
          (self#pexpr ~leading pCall) fun_
          (pp_list ",@ " (self#pexpr ~leading:false pAssignment)) args

    | J.Je_new (_, obj, args) ->
        pp f "@[new %a@[<hov 1>(%a)@]@]"
          (self#pexpr ~leading:false pMember) obj
          (pp_list ",@ " (self#pexpr ~leading:false pAssignment)) args

    | J.Je_hole (_, qml) -> (
        match qml with
        | Q.Const (_, (Q.String s)) ->
            (*
              This case is generated by bsl projections,
              because currently this generation is not done in
              full ast. This is a way to do a verbatim.
            *)
            pps f s

        | _ ->
            (*
              This case is used just for debugging, this is not
              meant to be printed as real js syntax.
              In practice, this is used only for tracking js passes,
              for the real js, this node is resolved at initialisation
              of the server, and no more present in the js.
            *)
            pp f "@[{HOLE[%a]}@]"
              QmlPrint.pp#expr qml
      )

    | J.Je_runtime (_, _) ->
        (* this case doesn't matter in command line *)
        Format.pp_print_string f "/* non printed runtime expr */"

  (*
    Used everywhere but at toplevel, for regrouping var definitions.
    function foo() {
      var x, y, z = 5;
    }
  *)
  method statements f (s : J.statement list) =
    let sep =
      let needed = ref false in
      (fun () ->
         if !needed then Format.fprintf f "@\n" ;
         needed := true
      )
    in
    let print acc =
      match List.rev acc with
      | [] -> ()
      | bindings ->
          let bind f (ident, expr) =
            match expr with
            | None ->
                self#ident f ident

            | Some expr ->
                pp f "%a =@;<1 2>%a"
                  self#ident ident
                  (self#pexpr ~leading:false pAssignment) expr
          in
          sep ();
          pp f "@[<hv 2>var@ %a;@]"
            (pp_list ",@ " bind) bindings
    in
    let rec aux acc = function
      | [] -> print acc
      | hd::tl -> (
          match hd with
          | J.Js_var (_, ident, expr) ->
              aux ((ident, expr)::acc) tl
          | _ ->
              print acc ;
              sep ();
              self#statement f hd ;
              aux [] tl
        )
    in
    aux [] s


  method block = self#statements

  method statement f (s: J.statement) =
    match s with
    | J.Js_var (_, ident, o) -> (
        match o with
        | None ->
            pp f "@[<hv 2>var %a;@]"
              self#ident ident
        | Some expr ->
            pp f "@[<hv 2>var %a =@ %a;@]"
              self#ident ident
              (self#pexpr ~leading:false pAssignment) expr
      )

    | J.Js_function (_, ident, params, body) ->
        pp f "@[<hv2>function %a@[<hv 1>(%a)@]{@\n%a@]@\n}"
          self#ident ident
          (pp_list ",@ " self#ident) params
          self#statements body

    | J.Js_return (_, expr) ->
        pp f "@[<h>return%a;@]"
          (Option.pp_sep " " (self#pexpr ~leading:false p)) expr

    | J.Js_continue (_, label) ->
        pp f "@[<h>continue%a;@]"
          (Option.pp_sep " " Format.pp_print_string) label

    | J.Js_break (_, label) ->
        pp f "@[<h>break%a;@]"
          (Option.pp_sep " " Format.pp_print_string) label

    | J.Js_switch (_, expr, cases, default) ->
        let pp_case f (expr, stmt) =
          pp f "@[<hv2>case %a:@\n%a@]"
            (self#pexpr ~leading:false p) expr
            self#statement stmt
        in
        let pp_default f stmt =
          pp f "@\n@[<hv2>default:@\n%a@]"
            self#statement stmt
        in
        pp f "@[<hv2>switch (%a) {@\n%a%a@]@\n}"
          (self#pexpr ~leading:false p) expr
          (pp_list "@\n" pp_case) cases
          (Option.pp pp_default) default

    | J.Js_if (_, cond, then_, None) ->
        pp f "@[<hv2>if (%a) {@\n%a@]@\n}"
          (self#pexpr ~leading:false p) cond
          self#statement then_

    | J.Js_if (_, cond, then_, Some else_) ->
        pp f "@[<hv2>if (%a) {@\n%a@]@\n@[<hv2>} else {@\n%a@]@\n}"
          (self#pexpr ~leading:false p) cond
          self#statement then_
          self#statement else_

    | J.Js_throw (_, expr) ->
        pp f "@[throw %a;@]"
          (self#pexpr ~leading:false p) expr

    | J.Js_expr (_, expr) ->
        pp f "@[%a;@]"
          (self#pexpr ~leading:true p) expr

    | J.Js_trycatch (_, body, catches, finally) ->
        let pp_catch f (ident, guard, stmt) =
          pp f "@[<hv2>} catch (%a%a) {@\n%a@]@\n"
            self#ident ident
            (Option.pp_sep " if " (self#pexpr ~leading:false p)) guard
            self#statement stmt
        in
        let pp_finally f stmt =
          pp f "@[<hv2>} finally {@\n%a@]@\n"
            self#statement stmt
        in
        pp f "@[<hv2>try {@\n%a@]@\n%a%a@\n}"
          self#statement body
          (pp_list "" pp_catch) catches
          (Option.pp pp_finally) finally

    | J.Js_for (_, init, cond, incr, body) ->
        let ppo = Option.pp (self#pexpr ~leading:false p) in
        pp f "@[<hv2>for @[<hv 1>(%a;@ %a;@ %a)@] {@\n%a@]@\n}"
          ppo init
          ppo cond
          ppo incr
          self#statement body

    | J.Js_forin (_, lhs, rhs, body) ->
        (* FIXME: priority isn't good *)
        pp f "@[<hv2>for @[<hv 1>(%a in %a)@] {@\n%a@]@\n}"
          (self#expr ~leading:false) lhs
          (self#expr ~leading:false) rhs
          self#statement body

    | J.Js_dowhile (_, body, cond) ->
        pp f "@[<hv2>do {@\n%a@]@\n} while (%a);"
          self#statement body
          (self#pexpr ~leading:false p) cond

    | J.Js_while (_, cond, body) ->
        pp f "@[<hv2>while (%a) {@\n%a@]@\n}"
          (self#pexpr ~leading:false p) cond
          self#statement body

    | J.Js_block (_, body) ->
        self#block f body

    | J.Js_with (_, expr, body) ->
        pp f "@[<hv2>with (%a) {@\n%a@]@\n}"
          (self#pexpr ~leading:false p) expr
          self#statement body

    | J.Js_label (_, label, stmt) ->
        pp f "@[<hv2>%s:{@\n%a@]@\n}"
          label
          self#statement stmt

    | J.Js_comment (_, `one_line, string) ->
        pp f "// %s" string

    | J.Js_comment (_, kind, string) ->
        pp f "%s@\n%s@\n*/"
          (match kind with `doc -> "/**" | _ -> "/*")
          string

  (*
    do not use self#statements there, all the toplevel would be
    merged in a big [var , , ] definition.
  *)
  method code f (c:J.code) =
    pp f "%a@\n" (pp_list "@\n" self#statement) c

end

class debug_printer =
object (self)
  inherit printer
  method block f body =
    pp f "@[<hv2>{@\n%a@]@\n}"
      self#statements body
end

class scoped_printer =
object(self)
  inherit printer as super
  method ident f i =
    match i with
    | J.ExprIdent e ->
        begin match Ident.safe_get_package_name e with
        | None -> pps f (safe_str (Ident.stident e))
        | Some name ->
            pp f "%s.%s"
              (safe_str name)
              (safe_str (Ident.stident e))
        end
    | J.Native (_, s) -> pps f s

  method private toplvl_statement f s =
    match s with
    | J.Js_var (_, ident, o) -> (
        match o with
        | None -> super#statement f s
        | Some expr ->
            match ident with
            | J.Native _ -> super#statement f s
            | J.ExprIdent i ->
                match Ident.safe_get_package_name i with
                | None -> super#statement f s
                | Some _ ->
                    pp f "@[<hv 2>%a =@ %a;@]"
                      self#ident ident
                      (self#pexpr ~leading:false pAssignment) expr
      )
    | J.Js_function (_, ident, params, body) -> (
        match ident with
        | J.Native _ -> super#statement f s
        | J.ExprIdent i ->
            match Ident.safe_get_package_name i with
            | None -> super#statement f s
            | Some _ ->
                pp f "@[<hv2>%a =@ function @[<hv 1>(%a)@]{@\n%a@]@\n}"
                  self#ident ident
                  (pp_list ",@ " self#ident) params
                  self#statements body
      )
    | _ -> self#statement f s

  method code f code =
    pp f "var %s = {};" (safe_str (ObjectFiles.get_current_package_name ()));
    pp f "%a@\n" (pp_list "@\n" self#toplvl_statement) code


end


let pp = new printer
let debug_pp = new debug_printer
let scoped_pp = new scoped_printer

let string_of_ident = Format.to_string pp#ident
let code = Format.to_string pp#code

module type X =
sig
  type lexem
  type t
  val append : t -> lexem -> t
  val empty : t
  (* *)

  val ident : string -> lexem
  val verbatim : string -> lexem
  val qml : QmlAst.expr -> lexem

  val serialized : JsAstRuntime.expr -> lexem list
end

module type S =
sig
  type t
  val code_elt : JsAst.code_elt -> t
end

module Make (X : X) =
struct
  type t = X.t

  type pending_lexem_kind = [
  | `qml
  | `ident
  | `lexems
  ]

  type pending_lexem = [
  | `qml of QmlAst.expr
  | `ident of string
  | `lexems of X.lexem list
  ]

  type acc = {

    (*
      The formatter created to print into, using the standard js printer
    *)
    formatter : Format.formatter ;

    (*
      A counter to know the current size of the pending_verbatim buffer.
      Used to push insertion hooks.
    *)
    counter : int ref ;

    (*
      Where the contains is actually printed.
      This contains is splitten at the end of the printing, building lexems.
    *)
    pending_verbatim : Buffer.t ;

    (*
      The lexems are generated and inserted at the end of the printing only.
    *)
    pending_lexems : pending_lexem Queue.t ;

    (*
      Insertion_hooks are pushing via formatter tags to tell the finalization where to insert
      the pending lexems.
    *)
    insertion_hooks : (int * pending_lexem_kind) Queue.t ;
  }

  let reset acc =
    Buffer.clear acc.pending_verbatim ;
    acc.counter := 0 ;
    Queue.clear acc.pending_lexems ;
    Queue.clear acc.insertion_hooks ;
    ()

  (*
    Finalize the acc, build the [X.t] structure, and reset the acc
  *)
  let finalize acc =
    Format.pp_print_flush acc.formatter () ;
    let pending_verbatim = acc.pending_verbatim in
    let pending_lexems = acc.pending_lexems in
    let insertion_hooks = acc.insertion_hooks in
    let length = Buffer.length pending_verbatim in
    let rec aux x_t pos =
      if Queue.is_empty insertion_hooks then (
        if not (Queue.is_empty pending_lexems) then assert false
        else
          let x_t =
            if pos >= length then x_t
            else
              let verbatim = Buffer.sub pending_verbatim pos (length - pos) in
              let lexem = X.verbatim verbatim in
              X.append x_t lexem
          in
          x_t
      )
      else if Queue.is_empty pending_lexems then assert false else (
        let hook, kind = Queue.take insertion_hooks in
        let () = if hook < pos then assert false in
        let verbatim = Buffer.sub pending_verbatim pos (hook - pos) in
        let x_t = X.append x_t (X.verbatim verbatim) in
        let x_t =
          let p_lexem = Queue.take pending_lexems in
          match kind, p_lexem with
          | `qml, `qml expr -> X.append x_t (X.qml expr)
          | `ident, `ident ident -> X.append x_t (X.ident ident)
          | `lexems, `lexems lexems -> List.fold_left X.append x_t lexems
          | _ -> assert false in
        aux x_t hook
      )
    in
    let x_t = aux X.empty 0 in
    reset acc ;
    x_t

  let init () =
    let pending_verbatim = Buffer.create 1024 in
    let counter = ref 0 in
    let pending_lexems = Queue.create () in
    let insertion_hooks = Queue.create () in
    let output s pos len =
      counter := !counter + len ;
      Buffer.add_substring pending_verbatim s pos len
    in
    let flush () = () in
    let formatter = Format.make_formatter output flush in
    let () =
      (*
        special traitment for tags
      *)
      let push_insertion_hook kind = Queue.push (!counter, kind) insertion_hooks ; "" in
      let mark_open_tag = function
        | "qml" -> push_insertion_hook `qml
        | "ident" -> push_insertion_hook `ident
        | "lexems" -> push_insertion_hook `lexems
        | _ -> "" (* other tags ignored *)
      in
      Format.pp_set_mark_tags formatter true ;
      Format.pp_set_formatter_tag_functions formatter {
        Format.
          mark_open_tag = mark_open_tag ;
          mark_close_tag = (fun _ -> "") ;
          print_open_tag = ignore ;
          print_close_tag = ignore ;
      } ;
    in
    {
      formatter = formatter ;
      counter = counter ;
      pending_verbatim = pending_verbatim ;
      pending_lexems = pending_lexems ;
      insertion_hooks = insertion_hooks ;
    }

  let ss_push_p_lexem acc lexem = Queue.push lexem acc.pending_lexems

  let ss_push_node name acc p_lexem =
    ss_push_p_lexem acc p_lexem ;
    let formatter = acc.formatter in
    Format.pp_open_tag formatter name ;
    Format.pp_close_tag formatter () ;
    ()

  let ss_push_ident acc ident =
    ss_push_node "ident" acc (`ident ident)
  let ss_push_qml acc qml =
    ss_push_node "qml" acc (`qml qml)
  let ss_push_lexems acc lexems =
    ss_push_node "lexems" acc (`lexems lexems)

  (*
    We use an object so that we can inherit of it, for redefining other versions.
    Format are used for hoping a possible indentation.
  *)
  class serializer(init) =
  object (self)
    inherit printer as super

    val acc = init

    method get_content = finalize acc

    method reset = reset acc

    method ident f (i : J.ident) =
      match i with
      | J.ExprIdent ident ->
          let ident = safe_str (Ident.stident ident) in
          ss_push_ident acc ident

      | J.Native (n, ident) -> (
          match n with
          | `global ->
              ss_push_ident acc ident
          | `local ->
              Format.pp_print_string f ident
        )

    method expr ~leading f ( e : JsAst.expr ) =
      match e with
      | J.Je_hole (_, Q.Const (_, Q.String s)) ->
          Format.pp_print_string f s
      | J.Je_hole (_, qml) ->
          ss_push_qml acc qml
      | J.Je_runtime (_, runtime_expr) ->
          let lexems = X.serialized runtime_expr in
          ss_push_lexems acc lexems
      | e ->
          super#expr ~leading f e

    method ss_code_elt ( elt : JsAst.code_elt ) =
      self#statement acc.formatter elt ;
      Format.fprintf acc.formatter "@\n" ;
      (*
        FIXME there: may or may not need to add a '\n'
        between statements, so that the js concatenation
        at runtime does not need to add a separator ?
      *)
      self#get_content

  end

  let ss = new serializer (init ())

  let code_elt elt =
    ss#ss_code_elt elt
end
