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
(**
   Opa and Qml compilers are based on this mechanism.
   We use this file as a tutorial and a memo because in qml and opa, the proportion
   of the asts may lead to be confusing about keeping in mind this global design.

   This file is a also a tiny example of how to use the passlib pass handler and track system.

   @author Mathieu Barbin
   @author Quentin Bourgerie
   @author Valentin Gatien-Baron
*)

(* you can search for 'open' in that file *)

module Arg = Base.Arg
module List = Base.List
module String = Base.String

module LangAst =
struct
  type expr =
    | A
    | B of expr * expr
    | K of int
    | Directive of (PassTracker.t * expr)

  type code_elt =
    | Val of string * expr
    | Package of [`declaration | `import] * string * FilePos.pos

  type code = code_elt list
end

module LangWalk =
struct
  module L = LangAst
  module Subs =
  struct
    type 'a t = LangAst.expr constraint 'a = _ * _ * _
    let foldmap tra acc t =
      match t with
      | L.A -> acc, t
      | L.B (a, b) ->
          let acc, a' = tra acc a in
          let acc, b' = tra acc b in
          acc,
          if a == a' && b == b' then t else
            L.B (a', b')
      | L.K _ -> acc, t
      | L.Directive (k, e) ->
          let acc, e' = tra acc e in
          acc,
          if e == e' then t else
            L.Directive (k, e')
    let iter x = Traverse.Unoptimized.iter foldmap x
    let map x = Traverse.Unoptimized.map foldmap x
    let fold x = Traverse.Unoptimized.fold foldmap x
  end
  module Expr = Traverse.Make2 ( Subs )
  module Elt_Subs =
  struct
    type 'a t = LangAst.expr constraint 'a = _ * _ * _
    type 'a container = LangAst.code_elt constraint 'a = _ * _ * _
    let foldmap tra acc t = match t with
    | L.Val (s, e) ->
        let acc, e' = tra acc e in
        acc,
        if e == e' then t else
          L.Val (s, e')
    | L.Package _ -> acc, t
    let iter tra = function
      | L.Val (_, e) -> tra e
      | L.Package _ -> ()
    let map x = Traverse.Unoptimized.map foldmap x
    let fold x = Traverse.Unoptimized.fold foldmap x
  end
  module Elt = Traverse.MakeLift1 (Elt_Subs) (Expr)
  module Code_Subs =
  struct
    type 'a t = LangAst.code_elt constraint 'a = _ * _ * _
    type 'a container = LangAst.code constraint 'a = _ * _ * _
    let iter = List.iter
    let map = List.map
    let fold = List.fold_left
    let foldmap = List.fold_left_map
  end
  module Code = Traverse.MakeLift2 (Code_Subs) (Elt)
  (* bring freds work there : ast projector *)
  (* usefull for meta opa (bsl ast projector) *)
  type 'proj projector =
      {
        a : 'proj ;
        b : 'proj -> 'proj -> 'proj ;
        k : int -> 'proj ;
        directive : PassTracker.t -> 'proj -> 'proj ;
      }
  let projection proj =
    let rec aux = function
      | L.A -> proj.a
      | L.B (a, b) -> proj.b (aux a) (aux b)
      | L.K i -> proj.k i
      | L.Directive (t, e) -> proj.directive t (aux e)
    in aux
  (* even more generical than the traverse *)
  (* if we define a foldmap_projection, you'll get
     the walker foldmap by applying it with LangCons
     as argument *)
end

module LangCons =
struct
  module L = LangAst
  let a = L.A
  let b a b = L.B (a, b)
  let k i = L.K i
end

module LangPrint =
struct
  module L = LangAst
  class print =
  object(self)
    method expr fmt = function
      | L.A -> Format.pp_print_char fmt 'A'
      | L.B (a, b) -> Format.fprintf fmt "@[<v2>B@ (%a)@ (%a)@]" self#expr a self#expr b
      | L.K i -> Format.pp_print_int fmt i
      | L.Directive (_, e) -> self#expr fmt e
    method code_elt fmt = function
      | L.Val (s, expr') -> Format.fprintf fmt "@[<2>val %s =@\n%a@]" s self#expr expr'
      | L.Package (`declaration, name, _) -> Format.fprintf fmt "package %s" name
      | L.Package (`import, name, _) -> Format.fprintf fmt "import %s" name
    method code fmt =
      List.iter (fun elt -> Format.fprintf fmt "%a@\n@\n" self#code_elt elt)
  end
  class bb_printer =
  object(self)
    inherit print as super
    method expr fmt = function
      | L.B (a, b) -> Format.fprintf fmt "@[<v2>BB@ (%a)@ (%a)@]" self#expr a self#expr b
      | e -> super#expr fmt e
  end
  let p = new print
  let expr = p#expr
  let code_elt = p#code_elt
  let code = p#code
end

module LangUtils =
struct
  module B =
  struct
    type 'a b_util = LangAst.expr -> LangAst.expr -> 'a
  end
end

module LangTrack =
struct
  module L = LangAst

  let trackers iter =
    LangWalk.Code.iter
      (function
         | L.Directive (t, e) -> iter.PassTracker.track (PassTracker.filename t) LangPrint.expr e
         | _ -> () )

  let vals iter = List.iter (
    function
      | ( L.Val (s, _) ) as v -> iter.PassTracker.track s LangPrint.code_elt v
      | _ -> ()
  )

  let code_id = PassHandler.define_printer "code"

  let printers = [
    code_id, LangPrint.code ;
  ]

  let trackers_id = PassHandler.define_tracker "trackers"
  let val_id = PassHandler.define_tracker "val"

  let trackers = [
    trackers_id, trackers ;
    val_id, vals ;
  ]
end

module LangError :
sig
  type context = (* or abstract *)
      {
        code_elt : LangAst.code_elt ;
        expr : LangAst.expr ;
      }
  val context : LangAst.code_elt -> LangAst.expr -> context
  val check_fail : PassHandler.cond_id -> context -> ('a, 'error) OManager.oformat -> 'a
  val scheck_fail : PassHandler.cond_id -> context -> ('a, unit) OManager.oformat -> 'a

  (** Overlaying PassHandler for located internal errors *)
  val cond_violation : PassHandler.cond_id -> context -> ('a, 'error) OManager.oformat -> 'a
  val scond_violation : PassHandler.cond_id -> context -> ('a, unit) OManager.oformat -> 'a
  val i_error : PassHandler.cond_id option -> context -> ('a, 'error) OManager.oformat -> 'a
  val i_serror : PassHandler.cond_id option -> context -> ('a, unit) OManager.oformat -> 'a

  (** Overlaying OManager for located errors *)
  (* val error : context -> ('a, unit, string, unit) format4 -> 'a *)
  (* val serror : context -> ('a, unit, string, unit) format4 -> 'a *)
  (* val warning : wclass:WarningClass.wclass -> context -> ('a, unit, string, unit) format4 -> 'a *)
  (* etc with the interface of OManager by partial call of the type (using internally OManager) *)
end =
struct
  type context =
      {
        code_elt : LangAst.code_elt ;
        expr : LangAst.expr ;
      }
  let context code_elt expr = { code_elt = code_elt ; expr = expr }
  let context_printer fmt context =
    Format.fprintf fmt "In the following toplevel definition :@\n%a@\nIn the following expression@\n%a\n" LangPrint.code_elt context.code_elt LangPrint.expr context.expr
  let console fmt context = Format.fprintf fmt "%a@\n" LangPrint.expr context.expr
  let check_fail c =
    PassHandler.check_fail ~full:context_printer ~console c

  let scheck_fail c =
    PassHandler.scheck_fail ~full:context_printer ~console c

  (* overlaying for passes *)
  let scond_violation c = PassHandler.scond_violation context_printer c
  let cond_violation c = PassHandler.cond_violation context_printer c
  let i_error c = PassHandler.i_error context_printer c
  let i_serror c = PassHandler.i_serror context_printer c
end

(* introduce some buggy scenario in the compiler (see documentation in arg.parse) *)
let bug_rewriting = ref false
let bug_allowed_cond_violation = ref false
let bug_cond_violation_contradiction = ref false
let bug_unbound_cond_violation = ref false
let bug_not_found = ref false

let current_filename = ref ""
let current_content = ref ""
let current_package_index = ref 1

let extrapaths = ref []

module LangCheck =
struct
  type ('env, 'a) checker = ('env -> 'a)  -> 'env PassHandler.cond
  module K_values :
  sig
    val positives : ('env, LangAst.code) checker
    val positives_id : PassHandler.cond_id
  end =
  struct
    module L = LangAst

    let (!!) code_elt expr = LangError.context code_elt expr

    let positives_id = PassHandler.define_cond (* WarningClass.cond_theme_example
                                                but there for testing : *) WarningClass.cond
    (* all K are positives *)
    let positives extract =
      PassHandler.make_condition positives_id
        (fun env -> List.iter
           (function
              | ( L.Val (_, e) ) as v ->
                  LangWalk.Expr.iter
                    (fun e -> match e with
                     | ( L.K i ) as e -> if !bug_cond_violation_contradiction then () else
                         if i < 0 then LangError.scheck_fail positives_id (!! v e) "positives check: %d < 0\n" i
                     | _ -> ()
                    ) e
              | _ -> ()
           )
           (extract env)
        )
  end

  module OtherTheme =
  struct
  end
end

module Lang =
struct
  module Ast = LangAst
  module Walk = LangWalk
  module Cons = LangCons
  (* module DeCons = LangDecons *)
  module Print = LangPrint
  (* module Parse = LangParse *)
  module Utils = LangUtils
  module Track = LangTrack
  module Error = LangError
  module Check = LangCheck
end

(* OUTSIDE OF THE KERNEL OF THE LANG *)

(* EXAMPLES OF PASSES *)

(* loading object *)
module LoadObjects =
struct
  module L = Lang.Ast
  let load code k =
    let extract_packages_decl = function
      | L.Package (kind, name, pos) -> Some (kind, name, pos)
      | _ -> None in
    let extract_more_deps _ = StringMap.empty in
    ObjectFiles.set_extrapaths ~no_stdlib:true !extrapaths;
    let l = [(!current_filename,!current_content,code)] in
    ObjectFiles.load ~no_stdlib:true extract_packages_decl extract_more_deps l
      (function
         | [(_,_,code)] -> k code
         | _ -> assert false)
end

(* alpha renaming *)
module AlphaConv =
struct
  module L = Lang.Ast
  type env = int IntMap.t * int

  (* separation *)
  module S =
  struct
    (* keep the bigger index used for renaming variables *)
    type t = int
    let pass = "AlphaConv"
    let pp = Format.pp_print_int
  end
  module R = ObjectFiles.Make(S)

  let initial () =
    let fold index t = max index t in
    let index = R.fold fold 0 in
    IntMap.empty, index

  let expr env =
    Lang.Walk.Expr.foldmap
      (fun ((env, index) as acc) -> function
         | L.K i -> if !bug_rewriting then acc, L.K (-1) else
             begin
               match IntMap.find_opt i env with
               | None ->
                   let env = IntMap.add i index env in
                   (env, succ index), L.K index
               | Some i ->
                   (env, index), L.K i
             end
         | t -> acc, t) env
  let code_elt e = Lang.Walk.Elt.foldmap_nonrec expr e
  let code e = Lang.Walk.Code.foldmap_nonrec expr e

  let pass c =
    let e = initial () in
    let (_, i), c = code e c in
    R.save i;
    c
end

(*
  dummy1 :
  + K 0 => A
*)
module Dummy1 =
struct
  module L = Lang.Ast
  let (!!) v e = Lang.Error.context v e
  let expr v =
    Lang.Walk.Expr.map_up
      (function
         | (L.K _) as t when !bug_cond_violation_contradiction ->
             Lang.Error.cond_violation Lang.Check.K_values.positives_id (!! v t) "contradiction"
         | (L.K i) as t ->
             (if !bug_allowed_cond_violation && (i < 0) then
                Lang.Error.cond_violation Lang.Check.K_values.positives_id (!! v t) "%d < 0" i
             );
             if i = 0 then L.A else t
         | t -> t)
  let code_elt code_elt = Lang.Walk.Elt.map_nonrec (expr code_elt) code_elt
  let code env = List.map code_elt env
end

(*
  dummy2
  + B (a, b) => B (9, b)
*)
module Dummy2 =
struct
  module L = Lang.Ast
  let (!!) v e = Lang.Error.context v e
  let expr v =
    Lang.Walk.Expr.map_up
      (function
         | (L.K i) as e ->
             (if !bug_unbound_cond_violation && (i < 0) then
                Lang.Error.scond_violation Lang.Check.K_values.positives_id (!! v e) "%d < 0" i
             );
             e
         | L.B (_, b) -> L.B (L.K (if !bug_rewriting then (-9) else 9), b)
         | t -> t)
  let code_elt code_elt = Lang.Walk.Elt.map_nonrec (expr code_elt) code_elt
  let code env = List.map code_elt env
end

module InsertTracker =
struct
  module L = Lang.Ast

  (* For the example, we add track at top level *)
  let insert = List.map
    (function
       | L.Val (id, e) ->
           let t = PassTracker.next id in
           L.Val (id, L.Directive (t, e))
       | c -> c
    )
end

(*
  Simulating the linking.
  In compilation mode: store the code.
  In linking mode: regroup all the code.
*)
module Linking =
struct

  module S =
  struct
    type t = LangAst.code
    let pass = "Linking"
    let pp f _ = Format.pp_print_string f "<dummy>"
  end
  module R = ObjectFiles.Make(S)

  let pass code =
    match ObjectFiles.compilation_mode () with
    | `compilation ->
        OManager.unquiet "compilation of package @{<bright>%S@} ok." !current_filename;
        R.save code;
        OManager.exit 0
    | `init ->
        code
    | `linking | `prelude ->
        let fold (package_name,pos) code t =
          OManager.verbose "linking with @{<bright>%s@}:%a" package_name FilePos.pp_pos pos ;
          code @ t in
        let full_parent_code = R.fold_with_name ~packages:true ~deep:true fold [] in
        let full_code = full_parent_code @ code in
        OManager.unquiet "linking ok.";
        full_code
end

module S3Passes =
struct
  module PH = PassHandler
  module L = Lang.Ast
    (* No parser, we use this : *)
  let code () =
    match !current_package_index with
    | 1 ->
        current_filename := "package-01.opa" ;
        current_content := String.random 10 ;
        [
          L.Package (`declaration, "package-01", FilePos.nopos "position of declaration package-01") ;
          L.Val ("toto", L.B (L.A, L.K 36)) ;
          L.Val ("titi", L.B (L.K 5,
                              L.B (L.K 4,
                                   L.B (L.A,
                                        L.B (L.A,
                                             L.B (L.K 5, L.K 0)
                                            )
                                       )
                                  )
                             )
                )
        ]
    | 2 ->
        current_filename := "package-02.opa" ;
        current_content := String.random 10 ;
        [
          L.Package (`declaration, "package-02", FilePos.nopos "position of declaration package-02") ;
          L.Package (`import, "package-01", FilePos.nopos "position of import package-01") ;
          L.Val ("toto", L.B (L.A, L.K 02)) ;
        ]
    | 3 ->
        current_filename := "package-03.opa" ;
        current_content := String.random 10 ;
        [
          L.Package (`declaration, "package-03", FilePos.nopos "position of declaration package-02") ;
          L.Package (`import, "package-02", FilePos.nopos "position of import package-02") ;
          L.Val ("toto", L.B (L.A, L.K 03)) ;
        ]
    | 4 ->
        (* simulating the final linking *)
        current_filename := "linking" ;
        current_content := "nothing" ;
        []
    | _ -> assert false

  let pass_Welcome () =
    PH.make_pass
      (fun _ ->
         OManager.printf "Prototype compiler for testing new pass system@\n";
         OManager.verbose "Compilation mode is %s" (
           match ObjectFiles.compilation_mode () with
           | `linking -> "linking"
           | `init -> "init"
           | `compilation -> "compilation"
           | `prelude -> "prelude"
         );
         PassHandler.init
      )

  let pass_Parse () =
    PH.make_pass
      (fun env ->
         { PH.
           env = code ();
           options = env.PH.options;
           printers = (fun _ -> Lang.Track.printers);
           trackers = (fun _ -> Lang.Track.trackers);
         })

  let pass_LoadObjects k =
    PH.make_pass
      (fun env ->
         let env = LoadObjects.load env.PH.env (fun code -> k ( { env with PH.env = code })) in
         PH.make_env () env
      )

  let pass_InsertTracker =
    PH.make_pass
      (fun env ->
         { env with
             PH.env = InsertTracker.insert env.PH.env
         })

  let pass_AlphaConv () =
    let extract s = s in
    let postcond =
      [
        Lang.Check.K_values.positives extract
      ] in
    PH.make_pass ~postcond
      (fun env ->
         let code = env.PH.env in
         let code = AlphaConv.pass code in
         { env with
             PH.env = code
         })

  let pass_NotFound = PH.make_pass (fun _ -> raise Not_found)

  let pass_Dummy1 =
    let extract s = s in
    let precond =
      [
        Lang.Check.K_values.positives extract
      ] in
    PH.make_pass ~precond
      (fun env ->
         { env with
             PH.env = Dummy1.code env.PH.env
         })

  let pass_Dummy2 =
    let extract s = s in
    let postcond =
      [
        Lang.Check.K_values.positives extract
      ] in
    PH.make_pass ~postcond
      (fun env ->
         { env with
             PH.env = Dummy2.code env.PH.env
         })

  let pass_Linking =
    PH.make_pass
      (fun env ->
         { env with
             PH.env = Linking.pass env.PH.env
         })

  let pass_Final _ =
    PH.make_pass (
      fun _ -> {
        PH.
          env = 0 ;
          options = ();
          printers =
            (fun _ -> [
               LangTrack.code_id,
               (fun fmt i ->
                  Format.pp_print_int fmt i ;
                  Format.pp_print_char fmt '\n'
               );
             ]);
          trackers = (fun _ -> []);
      }
    )

  let pass_Exit _ =
    PH.make_pass (
      fun env ->
        OManager.printf "exit %d\n" env.PH.env ;
        { PH.
            env = () ;
            options = ();
            printers =
              (fun _ -> [
                 LangTrack.code_id,
                 (fun fmt () ->
                    Format.fprintf fmt "Compilation if over -- no more code to print\n"
                 );
               ]) ;
            trackers = (fun _ -> []) ;
        }
    )
end

module Main =
struct
  module PH = PassHandler
  module S3 = S3Passes
  let (|>) a f = f a

  let if_not_found ~options:_ _ = !bug_not_found

  (* Normal style *)
  let normal () =
    PH.init
    |> PH.handler "Welcome" (S3.pass_Welcome())
    |> PH.handler "Parse" (S3.pass_Parse())
    |> PH.handler "LoadObjects" (S3.pass_LoadObjects (fun e -> e
      |> PH.handler "InsertTracker" S3.pass_InsertTracker
      |> PH.handler "AlphaConv" (S3.pass_AlphaConv())
      |> PH.if_handler ~if_:if_not_found "NotFound" S3.pass_NotFound
      |> PH.handler "Dummy1" S3.pass_Dummy1
      |> PH.handler "Dummy2" S3.pass_Dummy2
      |> PH.handler "Linking" S3.pass_Linking
      |> PH.handler "Final" (S3.pass_Final())
      |> PH.handler "Exit" (S3.pass_Exit())
      |> PH.return ))
    |> PH.return

  (* Binary operator style *)
  let (|?>) = PassHandler.(|?>)
  let (|+>) = PassHandler.(|+>)

  let binop () =
    PH.init
    |+> ("Welcome"       , S3.pass_Welcome()     )
    |+> ("Parse"         , S3.pass_Parse()       )
    |+> ("LoadObjects"   , S3.pass_LoadObjects (fun e -> e
      |+> ("InsertTracker" , S3.pass_InsertTracker )
      |+> ("AlphaConv"     , S3.pass_AlphaConv()   )
      |?> (if_not_found,
           "NotFound"      , S3.pass_NotFound      )
      |+> ("Dummy1"        , S3.pass_Dummy1        )
      |+> ("Dummy2"        , S3.pass_Dummy2        )
      |+> ("Linking"       , S3.pass_Linking       )
      |+> ("Final"         , S3.pass_Final()       )
      |+> ("Exit"          , S3.pass_Exit()        )
      |> PH.return  ))
    |> PH.return
end

let _ =
  let bugs =
    [

      "--bug1",
      Arg.Set bug_rewriting,
      " Introduce some bug in a rewriting rule of the compilation. If the check is activated, this is detected as post condition failure, if not, nothing appends";


      "--bug2",
      Arg.Set bug_allowed_cond_violation,
      " Introduce a cond violation detection during the compilation. If the check is activated, the pass is not executed, if not, the check is executed afterwards. Should be combined with --bug1 or not to test";


      "--bug3",
      Arg.Set bug_cond_violation_contradiction,
      " Introduce a cond violation during the compilation, but so that for the checker point of vue the condition is not violated. If the check is activated, the pass is executed, if not the check is executed afterwards, in both case an explanation is given";


      "--bug4",
      Arg.Set bug_unbound_cond_violation,
      " Introduce a rule declaring a cond violation where the cond is not in its preconditions should be combined with --bug1";


      "--bug5",
      Arg.Set bug_not_found,
      " Introduce an uncaught exception during compilation (Not_found)";

      "--file",
      Arg.Symbol (["1"; "2"; "3"; "4"],
                  (fun s ->
                     current_package_index := int_of_string s
                  )),
      " Tell what file to compile (testing separated compilation)"
      ;

      "-I",
      Arg.String (fun s -> List.iter (fun p -> extrapaths := p :: !extrapaths) (Arg.split s)),
      " Add extra path for loading packages"
      ;

    ] in

  let _ =
    Arg.parse
      (Arg.align
         (bugs
          @ ObjectFiles.Arg.public_options
          @ ObjectFiles.Arg.private_options
          @ PassHandler.Arg.options
          @ WarningClass.Arg.options
          @ OManager.Arg.options
         ))
      ignore "passdesign" in
  Main.binop ()
