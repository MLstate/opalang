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
(** a simple debug tracer for libqml & depends
    @author Mathieu Barbin - samedi 18 avril 2009, 15:19:34 (UTC+0100) *)

(**
    lundi 1 juin 2009, 23:10:42 (UTC+0100)
    to be sure that it will not be any debug interface for release version, the DebugTracer is not available
    if the mode mlstate_debug is not activated.
*)

val now : ?time:float -> unit -> string
val year : unit -> string
val htmlescaped : string -> string

#<< type data = string (** can be later Lazy, but YAGNI *) >>#;
#<< module Debug : (** interface type is common for every tracers *) >>#;
#<< sig >>#;
#<<   type id >>#;
#<<   type t = string * data list (** each module could say something about a topic-item *) >>#;
#<<   type tree = (id * t list) list (** for each topic, has a list of t *)  >>#;
#<<   val create : string -> id >>#;
#<<   val compare : id -> id -> int >>#;
#<<   val to_string : id -> string >>#;
#<<   val warning : id >>#;
#<<   val error : id >>#;
#<<   val info : id >>#;
#<< end >>#;
#<<  >>#;

#<< (** Extending the available tracers *)  >>#;
#<<  >>#;
#<< (** a specialized module to produce a diagnostic file *)  >>#;
#<< module type SPEDEBUGTRACER =  >>#;
#<< sig   >>#;
#<<   val ext : string   >>#;
#<<   val generator : libname:string -> libversion:string -> Debug.tree -> string * string (** returns (extension, contents) *)  >>#;
#<< end  >>#;
#<<    >>#;
#<< (** We do a Module.fold_left with all specialized tracer, to produce the final tracer *)  >>#;
#<< module type DEBUGTRACER =  >>#;
#<< sig   >>#;
#<<   val ext : string list (** accumulation of specialized extension *)  >>#;
#<<   val generate : libname:string -> libversion:string -> Debug.tree -> (string * string) list (** accumulation of specialized productions *)  >>#;
#<< end                 >>#;
#<<  >>#;
#<< (** Module.fold_left *)  >>#;
#<< module EmptyTracer : DEBUGTRACER  >>#;
#<< module AddTracer : functor (Tracer : DEBUGTRACER) -> functor (Spe : SPEDEBUGTRACER) -> DEBUGTRACER  >>#;

#<< (** The current spe-tracer(s) & the tracer build by adding it to the empty tracer *) >>#;
#<< module HTMLTracer : SPEDEBUGTRACER >>#;
#<< module DebugTracer : DEBUGTRACER >>#;

(** The main module *)
module type DEBUGINTERFACE =
sig
  (** In any case : Reduced interface is colored messages only *)
  val error : ?ending:(string -> 'a) -> ?color:Ansi.color -> string -> string -> 'a
  val warning : string -> ?color:Ansi.color -> string -> unit
  val verbose : string -> ?color:Ansi.color -> string -> unit
  val withcolor : bool -> unit (** default is Parameter.withcolor *)

  (** whisper : print something to stdout only if quiet = true with no `plot` (= no extra transformation on the string) *)
  val whisper : string -> unit
#<<   val debug : string -> Debug.id -> string -> unit >>#;
#<<   val set_trace_prefix : string -> unit >>#;
#<<   val trace : ?verbose:bool -> unit -> unit (** default is verbose:true *) >>#;
#<<   val suspend : unit -> unit >>#;
#<<   val active : unit -> unit (** start is unactive *) >>#;
#<<   val is_active : unit -> bool >>#;
end

module type INTERFACEPARAMETER =
sig
  val libname : string
  val version : string
  val quiet : unit -> bool
  module DefaultColor :
  sig
    val error : Ansi.color
    val warning : Ansi.color
    val verbose : Ansi.color
    val withcolor : bool
  end
end

module MakeDebugInterface :
  functor (InterfaceParameter : INTERFACEPARAMETER) ->
#<<     functor (DebugTracer : DEBUGTRACER) ->  >>#;
      DEBUGINTERFACE

(* (\** The instanciation of a such interface for the libqml -- with the html tracer only as DebugTracer *\) *)
(* module LibqmlDebugInterface : DEBUGINTERFACE *)

(* (\** Facilities for acces to the LibqmlDebugInterface *\) *)
(* include DEBUGINTERFACE *)
