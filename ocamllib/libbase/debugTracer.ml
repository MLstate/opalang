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
(** implementation of debugTracer : cf mli file
    @author Mathieu Barbin samedi 18 avril 2009, 15:35:35 (UTC+0100) *)

let tm () = Unix.localtime (Unix.time ())

let d2 n =
  let d = n / 10 and u = n mod 10 in
  Printf.sprintf "%d%d" d u

let year () =
  let t = Unix.localtime (Unix.time ()) in
  ("20"^(d2 (t.Unix.tm_year mod 100)))

let now ?time () =
  let t = Unix.localtime (match time with Some t -> t | _ -> Unix.time ()) in
  Printf.sprintf "%s/%s/%s - %s:%s:%s"
    (d2 t.Unix.tm_mday)
    (d2 (t.Unix.tm_mon + 1))
    (d2 (t.Unix.tm_year mod 100))
    (d2 t.Unix.tm_hour)
    (d2 t.Unix.tm_min)
    (d2 t.Unix.tm_sec)

let htmlescaped =
  let f = function
    | '<' -> "&lt;" | '>' -> "&gt;" | '&' -> "&amp;" | '\'' -> "&apos;" | '\"' -> "&quot;" | c -> String.make 1 c in
  fun s ->
    let len = String.length s in
    (*     let rec fold buf i = if i >= len then FBuffer.contents buf else fold (FBuffer.add buf (f (String.unsafe_get s i))) (succ i) in *)
    (*     fold (FBuffer.create 1024) 0 *)
    let rec fold buf i =
      if i >= len then
        FBuffer.contents buf
      else
        let c = String.unsafe_get s i in
        if c = '\027' && i + 3 < len && (String.unsafe_get s (i+1)) = '[' then
          if (String.unsafe_get s (i+2)) = '0' then
            fold (FBuffer.add buf "</span>") (i+4)
          else
            let color_type = if (String.unsafe_get s (i+2)) = '4' then "background" else "foreground" in
            let color_name = Ansi.string_of_color (Ansi.uncolor (int_of_string (String.make 1 (String.unsafe_get s (i+3))))) in
            fold (FBuffer.add buf (Printf.sprintf "<span class=\"%s_color_%s\">" color_type color_name)) (i+5)
        else
          fold (FBuffer.add buf (f c)) (i+1)
    in
    fold (FBuffer.create 1024) 0

#<< type data = string >>#;
#<< module Debug = >>#;
#<< struct >>#;
#<<   type id = int * string >>#;
#<<   type t = string * data list >>#;
#<<   type tree = (id * t list) list >>#;
#<<   let fresh = let t = ref (-1) in (fun s -> incr(t); (!t, s)) >>#;
#<<   let compare (a, _) (b, _) = Pervasives.compare a b >>#;
#<<   let create = fresh >>#;
#<<   let to_string (_, s) = s >>#;
#<<   let warning = fresh "warnings" >>#;
#<<   let error = fresh "errors" >>#;
#<<   let info = fresh "infos (verbose)" >>#;
#<< end >>#;
#<< module type DEBUGTRACER = sig val ext : string list val generate : libname:string -> libversion:string -> Debug.tree -> (string * string) list end >>#;
#<< module type SPEDEBUGTRACER = sig val ext : string val generator : libname:string -> libversion:string -> Debug.tree -> (string * string) end >>#;
#<< module EmptyTracer : DEBUGTRACER = struct let ext = [] let generate ~libname:_ ~libversion:_ _ = [] end >>#;
#<< module AddTracer (Tracer : DEBUGTRACER) (Spe : SPEDEBUGTRACER) : DEBUGTRACER = >>#;
#<< struct >>#;
#<<   let ext = Spe.ext::Tracer.ext >>#;
#<<   let generate ~libname ~libversion tree = (Spe.generator ~libname ~libversion tree)::(Tracer.generate ~libname ~libversion tree) >>#;
#<< end >>#;
#<< module HTMLTracer : SPEDEBUGTRACER = >>#;
#<< struct >>#;
#<<   let _begin = " >>#;
#<< <html> >>#;
#<<  <head> >>#;
#<<   <title>debug output - MLstate (c) 2009</title> >>#;
#<<   <link rel=\"stylesheet\" type=\"text/css\" href=\"/shared/debugstyle.css\" /> >>#;
#<<   <link rel=\"stylesheet\" type=\"text/css\" href=\"/shared/ocaml/lib/debugstyle.css \" /> >>#;
#<<   <link rel=\"stylesheet\" type=\"text/css\" href=\"/shared/ocaml/lib64/debugstyle.css \" /> >>#;
#<<   <link rel=\"stylesheet\" type=\"text/css\" href=\"debugstyle.css\" /> >>#;
#<<  </head> >>#;
#<<  <body> >>#;
#<< "  >>#;
#<<   let uib = Printf.sprintf "<u><i>%s</i></u>" >>#;
#<<   let cont = uib "back to contents" let ppred = uib "pred" let nnext = uib "next" >>#;
#<<   let href = Printf.sprintf "<a href=\"#%d\">%s</a>" >>#;
#<<   let name = Printf.sprintf "<a name=\"%d\">%s</a>" >>#;
#<<   let hh i s = Printf.sprintf "<h%d>%s</h%d>" i s i >>#;
#<<   let _end = " >>#;
#<<  </body> >>#;
#<< </html>" >>#;
#<<   let ext = "html" >>#;
#<<   let labelm = let t = ref 0 in fun () -> incr(t); !t >>#;
#<<   let generator ~libname ~libversion tree = >>#;
#<<     let fold_item (pp, buf, link) (m, lmess) = >>#;
#<<       let label = labelm () in >>#;
#<<       let buf = FBuffer.addln buf (Printf.sprintf "<li>%s %s %s %s <pre>" (name label ("Module "^(String.capitalize m)))  (href pp ppred) (href (succ label) nnext) (href 0 cont)) in >>#;
#<<       let buf = List.fold_left (fun buf m -> FBuffer.addln buf (htmlescaped m)) buf lmess in >>#;
#<<       let buf = FBuffer.addln buf "</pre></li>" in >>#;
#<<       let link = FBuffer.addln link (Printf.sprintf "<li>%s</li>" (href label ("module "^(String.lowercase m)))) in >>#;
#<<       label, buf, link in >>#;
#<<     let fold_id (buf, link) (id, items) =  >>#;
#<<       let label = labelm () and idd = Debug.to_string id in >>#;
#<<       let buf = FBuffer.addln buf ((name label (hh 2 (String.capitalize idd)))^"<ul>") in >>#;
#<<       let link = FBuffer.addln link ((href label (String.lowercase idd))^"<ul>") in >>#;
#<<       let _, buf, link = List.fold_left fold_item (pred label, buf, link) items in >>#;
#<<       let buf = FBuffer.addln buf "</ul>" and link = FBuffer.addln link "</ul>" in buf, link in >>#;
#<<     let tree_debug, link = List.fold_left fold_id ((FBuffer.create 1024), (FBuffer.create 1024)) tree in >>#;
#<<     ext,  >>#;
#<<     List.fold_left (^) "" >>#;
#<<       [ >>#;
#<<     _begin; >>#;
#<<     Printf.sprintf "<h1>Debug Tracer Interface for %s version %s</h1>" libname libversion; >>#;
#<<     Printf.sprintf "<h2>Date of this diagnosis : %s</h2>\n" (now ()); >>#;
#<<     "<small>\n"; >>#;
#<<     name 0 (hh 4 "contents :"); >>#;
#<<     FBuffer.contents link; >>#;
#<<     "</small>\n"; >>#;
#<<     FBuffer.contents tree_debug; >>#;
#<<     _end   >>#;
#<<       ] >>#;
#<< end >>#;
#<< module DebugTracer : DEBUGTRACER = AddTracer(EmptyTracer)(HTMLTracer) >>#;
module type DEBUGINTERFACE =
sig
  val error : ?ending:(string -> 'a) -> ?color:Ansi.color ->  string -> string -> 'a
  val warning : string -> ?color:Ansi.color -> string -> unit
  val verbose : string -> ?color:Ansi.color -> string -> unit
  val withcolor : bool -> unit
  val whisper : string -> unit

#<<   val debug : string -> Debug.id -> string -> unit >>#;
#<<   val set_trace_prefix : string -> unit >>#;
#<<   val trace : ?verbose:bool -> unit -> unit >>#;
#<<   val suspend : unit -> unit >>#;
#<<   val active : unit -> unit >>#;
#<<   val is_active : unit -> bool >>#;
end
module type INTERFACEPARAMETER =
sig
  val libname : string val version : string val quiet : unit -> bool
  module DefaultColor :
    sig
      val error : Ansi.color
      val warning : Ansi.color
      val verbose : Ansi.color
      val withcolor : bool
    end
end
(** We got it : thanks to Mehdi who find the info on ocaml logs that ocaml has random bug with systhread *)
module MakeDebugInterface (P : INTERFACEPARAMETER)
#<<   (DebugTracer : DEBUGTRACER)  >>#;
  =
 struct
#<<    (* imperative structurs of logs *)  >>#;
#<<    let this_id = Printf.sprintf "%s-DebugInterface" P.libname  >>#;
#<<    module DebugTable :  >>#;
#<<    sig  >>#;
#<<      val add : Debug.id -> string -> string -> unit  >>#;
#<<      val build_tree : unit -> Debug.tree  >>#;
#<<      val reset : unit -> unit  >>#;
#<<    end =  >>#;
#<<    struct  >>#;
#<<      let _table = SortHashtbl.create 10  >>#;
#<<      let reset () = SortHashtbl.clear _table    >>#;
#<<      let init_module mess = [mess]  >>#;
#<<      let init_id id = SortHashtbl.add _table id (SortHashtbl.create 10)   >>#;
#<<      let add id mo mess =  >>#;
#<<        match SortHashtbl.find_opt _table id with  >>#;
#<<        | None ->   >>#;
#<<       let items = SortHashtbl.create 10 in  >>#;
#<<       let allmess = init_module mess in  >>#;
#<<       SortHashtbl.replace items mo allmess;  >>#;
#<<       SortHashtbl.replace _table id items  >>#;
#<<        | Some items ->  >>#;
#<<       let was = Option.default [] (SortHashtbl.find_opt items mo) in  >>#;
#<<       SortHashtbl.replace items mo (mess::was)  >>#;
#<<      let build_tree () =  >>#;
#<<        let fold_item mo allmess ac = (mo, List.rev allmess)::ac in  >>#;
#<<        let fold id items ac =  >>#;
#<<     let mitems = SortHashtbl.fold_right fold_item items [] in  >>#;
#<<     (id, mitems)::ac in  >>#;
#<<        SortHashtbl.fold_right fold _table []  >>#;
#<<      let _ = List.iter init_id [Debug.error; Debug.warning; Debug.info]  >>#;
#<<    end     >>#;
   let _with_color = ref P.DefaultColor.withcolor
   let withcolor t = _with_color := t
#<<    let suspend, active, is_active =  >>#;
#<<      let _state = ref false in  >>#;
#<<      (fun () -> _state := false), (fun () -> _state := true), (fun () -> !_state)    >>#;
   let plot
#<<        debugid  >>#;
       prefix _module ?(color=`black) m =
     let message = Base.String.replace m "\n" "\n\t" in
#<<      (if is_active () then DebugTable.add debugid _module message);  >>#;
     let m = Printf.sprintf "%s%s : %s" prefix _module message in
     if !_with_color then Ansi.print color m else m
   let warning _mo ?(color=P.DefaultColor.warning) m =
     if P.quiet () then
       ()
     else
     prerr_endline (plot
#<<                   Debug.warning  >>#;
                      "warning " _mo ~color m)
   let verbose _mo ?(color=P.DefaultColor.verbose) m =
     if P.quiet () then
       ()
     else
     prerr_endline (plot
#<<                   Debug.info  >>#;
                      "" _mo ~color m)
   let verboze = verbose
     (* partial application for modules -- including thread denomination *)
#<<    let debug mo id m = if is_active () then  >>#;
#<< (*      let extrath = let t = Thread.id (Thread.self ()) in if t = 0 then "" else Printf.sprintf "[th-%d]: " t in  *) >>#;
#<<      DebugTable.add id mo (Base.String.replace m "\n" "\n\t")  >>#;
#<<    let plurial, debug_ext = match DebugTracer.ext with  >>#;
#<<    | [t] -> "", "."^t  >>#;
#<<    | _::_ as l -> "s", "{"^(String.concat ", " l)^"}"  >>#;
#<<    | _ -> failwith (Printf.sprintf "%s has no tracer-module implemented, and that is sad !" this_id)  >>#;
#<<    let _prefix = ref ((String.lowercase P.libname)^"diagnostic")  >>#;
#<<    let set_trace_prefix s = _prefix := s  >>#;
#<<    let trace ?(verbose=true) () =  >>#;
#<<      (if verbose then verboze this_id (Printf.sprintf "for more debug info, see the file%s %s%s generated for you" plurial !_prefix debug_ext));  >>#;
#<<      let tree = DebugTable.build_tree () in   >>#;
#<<      let debug_files = DebugTracer.generate ~libname:P.libname ~libversion:P.version tree in  >>#;
#<<      List.iter (fun (ext, contents) ->  >>#;
#<<              let filename = !_prefix^"."^ext in  >>#;
#<<              try let oc = open_out filename in output_string oc contents; close_out oc with  >>#;
#<<                _ -> verboze this_id ~color:P.DefaultColor.error (Printf.sprintf "cannot generate debug-diagnosis file %s" filename)  >>#;
#<<           ) debug_files  >>#;
   let error ?(ending=fun (_:string) -> exit 1) ?(color=P.DefaultColor.error) _mo m =
     prerr_endline (plot
#<<                   Debug.error  >>#;
                      "[!] " _mo ~color m); ending m
#<<    let _ = at_exit (fun () -> if is_active () then trace ())  >>#;

   let whisper m =
     if P.quiet () then
       ()
     else
       print_endline m
 end
