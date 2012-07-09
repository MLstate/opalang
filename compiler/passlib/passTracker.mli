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

(**
   Tracking system for following transformations during passes.

   @author Mathieu Barbin
   @author Quentin Bourgerie
   @author Valentin Gatien-Baron
*)

(** {6 Types alias} *)
(** *)
type passname = string
type printer_id = string
type tracker_id = string
type filename = string

(** the type of a pprinter *)
type 'a printer = Format.formatter -> 'a -> unit

(** the type of an outputer *)
type 'a outputer = Pervasives.out_channel -> 'a -> unit

(** {6 Trackers and attached values} *)

(** The abstract type for a track *)
type t

(** The info attached by default to a track *)
type info = string

(** Get the info attached to a track *)
val info : t -> info

(** Generate a fresh track *)
val next : info -> t

(** Get the filename associated to a tracker ["index.%d"] where d is a uniq index *)
val filename : t -> string

(**
   Generalized tracker iterator: usable with other type than [t]
   The function is embedded in a record so that we can explicitly
   generalize its type, and have then not homogeneous trackers in a
   same list without type checking problem about generalization.
 *)
type iter_tracker = {
  track : 'tracked. (filename -> 'tracked printer -> 'tracked -> unit) ;
}

(** The type of a generic tracks printer *)
type 'env tracker = iter_tracker -> 'env -> unit

(** {6 Utilisation from Opa Compiler} *)

(**
   The standard example:

   In the ast, you'll find a directive
   {[Directive ( `tracker of PassTracker.t , [ expr ] , None )]}

   Every pass should traverse this directive, considering it as a decoration only.
   The directive should not be removed, and the rewriting pass should rewrite the
   [expr], as usual. The type of the directive is ['a -> 'a].

   The [PassHandler] module will ask for a [iter_tracker] function, which
   should be something like :

   {[
   (* The standard printer of your tracked expressions *)
   val fmt : 'expr PassHandler.printer
   ...
   let iter iter env =
     LangWalk.Code.iter
       (function
         | Directive (`tracker t, [e], _) -> iter.track (PassTracker.filename t) fmt e
         | _ -> () ) (extract_code env)
   ]}

   It will update the track system file, and update the system file system tree.

   Default tracking is used for debug, but we can use trackers for other issues
   (any information extraction during the compilation).

   Other example:

   You can track all toplevel value of a code
   {[
   let iter iter env =
     List.iter (function
      | NewVal b | NewVal b -> List.iter (fun (s, e) -> iter.track (ExprIdent.stident s) fmt e) b
      | _ -> () ) (extract_code env)
   ]}

   Tracked values are then regrouped in a sub-folder of the [track] directory.
*)

(** {6 Interaction with PassHandler} *)

(**
   Error if the output has already began (default is ["_track"])
*)
val set_directory : string -> unit

(**
   Get the directory of tracking. If the directory given was relative, and the tracking has
   already began, will return an absolute path.
*)
val get_directory : unit -> string

(**
   Open a file in the system directory hierarchy, and outputs the ['env] in the file using
   the given printer. Will also update the .list files

   Convention, all passes should produce the same filename for the same semantic, but
   passes could outputs more than one file.

   The [printer_id] is a filename, and it is by convention the name of the subdir
   of [printers] in which logs are stored.

   e.g. ["code", "gamma", "types", etc...]
*)
val print : passname:passname -> printer_id:printer_id -> 'env printer -> 'env -> unit

(**
   The current pass wants to output a file.
   Open a file in the system directory hieararchy, and outputs the ['env] in the file using
   the printer. This is used for more free generation of ad-hoc files.
   The function returns the path to the filename. (e.g. for lunching viewer during compilation, etc...

   If this function is called several time during the same pass, the
   file is re-open in append mode (not overwrited)
*)
val file : filename:filename -> 'env outputer -> 'env -> filename

(**
   Producing a log for a tracker.
   The [PassHandler] module should use this function with a partial call, inserting
   the name of the pass as argument [pass], and the name of the tracker as subdir.
   The [tracker_id] is a filename, and it is by convention the name of the subdir
   of [trackers] in which logs are stored.
*)
val track : passname:passname -> tracker_id:tracker_id -> 'env tracker -> 'env -> unit

(**
   Producing a trace of timing for a pass.
   The [PassHandler] module should use this function after its computation,
   with the time taken by the pass to make the transformation, without taking
   in consideration printing, checking, tracking, etc...
*)
val time : passname:passname -> float -> unit

(**
   The current pass wants to add some more specific logs.
   This log will be stored in the pass directory and be accessible with
   the internal_error mode of opatrack.
*)
val internal : filename:filename -> 'env printer -> 'env -> unit

(**
   Used for outputting error report of checkers. The given filename must be the name of the [cond_id].
   If the pass is not specified, the logs go in default/check/
*)
val check_fail : filename:filename -> 'env printer -> 'env -> unit

(**
   [PassHandler] should use this function to tell what pass is running,
   so that [internal] knows where to store files for the internal rule.
   If the pass is not specified, the logs go in default/internal/
*)
val set_current_passname : passname -> unit

(**
   Used for saving and restoring environments to avoid redoing the whole compilation
   when you want to look at the end of the compilation
*)
val marshal : passname:passname -> 'a -> unit
val unmarshal : passname:passname -> 'a

(**
   If you need to marshal more that just the environment given to the pass
   (for instance OpaMapToIdent), you can register it here and it will be saved
   and restored just like the rest of the environment
*)

val register_global_env : ('a -> unit) * (unit -> 'a) -> unit
(** register a global state, given a function to write to it and a function to read it *)
val register_global_ref : _ ref -> unit

(** {6 File Organisation} *)

(**
   The output files are meant to be organized so that externs applications
   can make easier the analysis of traces, and the automation of launching
   meld-like applications. [opatrack] is a bash script with several modes dedicated
   to this file system.

   {[
   passes.list
   printers.list
   trackers.list

   trackers/
     tracker.list
     other.list

   time/
     pass_Name.time
     pass_OtherPass.time

   pass_Name/
     printers/
       some_file
       an_other_file
     trackers/
       tracker/
         index.1
         index.2
         etc...
       other/
         something
         whatever
     internal/
       some_internal_log
       some_other
     check/
       cond.cat.this.%d
     files/
       some_more_file

   pass_OtherPass/
     printers/
       some_file
       an_other_file
       an_extra_file
     trackers/
       tracker/
         index.1
         index.2
         etc...
       etc..
   etc...
   ]}

   Files *.list are always generated, even if they are empty.

   {b passes.list} is the list of the folder named [pass_*] in the file system.
   The file is ordered as passes are ordered in the compilation process.
   1 name per line, no blank line, the name of the pass is the name of
   the directory.

   {b printers.list} is the list of files which have been output at some point
   by a pass. if a file is in [printers.list], that means that it must be
   in some pass directory a file with this name.

   Usually, passes output to a file with the same name between passes,
   for tracking the evolution of something during the compilation.
   Typical use, is [code] containing the printed code.

   The file is ordered as passes have generated files during the compilation
   process. 1 name per line, no blank line.

   {b trackers.list} is the same idea, for files.
   track files are by convention named [index.%d] where the extension design
   the uniq index associated to a value of type [PassTracker.t]

   It is yet not frozen about what we will attach to tracker. Currently this
   is a string. This string may be available directly in the file index.list,
   {[
   cut -d' '      # will give you the index
   cut -d' ' -f2- # will give you the info
   ]}
   or maybe later, we could produce directly [index.%d.info] files, with
   larger attached infos (e.g. one field per line)
*)

(* TODO: investigate:
   + unzoom printer based on tracker population le printer gros grain.
   + usability for a simple opadoc.
   + exporting Arg.parse support directly in the module.
*)

(** {6 Runtime Tags} *)

(**
   Runtime support for dynamic attached values.
   It is used to set/get values from trackers.

   <!> Not for casual user. Please ask the team before using it.
*)

(**
   Generate a fresh get/set couple for embed values in a tracker.

   That looks like magic but it is safe :
   if you try to [get] from a [t] you do not have embeded with
   the [set] function, you'll get a [None].
*)
val embed : unit -> (t -> 'a -> unit) * (t -> 'a option)
