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
   OpaTop Bypass Management, binding with libbsl.
   @author Mathieu Barbin
*)

(**
   Unlike compilers which uses plugins containing only essentially
   string like informations about primitives (like the function name,
   it's type, etc...), the interpreter needs to access a dynamic pointer
   on the implementation of the function.

   This is possible with libbsl, that is what {b loaders} are for.
   Loaders contain the same informations then the Plugins, but with
   an extra [Obj.t] : the implementation of the primitive.

   In the opatop main, we first store all linked loaders in the
   common [BslPluginTable], using explicitelly their function
   [self_store], so that we are sure that they are linked with
   the executable.

   After that we execute the main loop of the interpreter,
   which begins with an initial building of the starting environment.
   At this point, this module is requested to build the bymap,
   which correspond to registering and exporting all primitives from
   the loaders stored into the [BslPluginTable].
*)

(**
   The manipulation of bypasses has been simplified, and opatop
   use the standard bsl, [BslLib.BSL].
*)

type bypass_map = BslLib.BSL.ByPassMap.t
type bypass     = BslLib.BSL.ByPass.t

(**
   Building the bymap from the contents of the [BslPluginTable].
   Do not call this function <<too soon>>, or the map will be empty.
   - [too soon] means before the linked loaders may have time to
   store them self in the [BslPluginTable].

   In practice, whenever we need to build a new toplevel, we generate
   a main which look like :
   {[
   let _ =
      Loader1.self_store () ;
      Loader2.self_store () ;
      start_main_of_opatop ()
   ]}

   There is a cache for the built bypass_map, so that you can call this
   function whenever you need to access the bypass_map.
*)
val bypass_map : unit -> bypass_map

(**
   Once the map has been build, we can ask for a bypass key to be resolved.
   If the bypass is unknown, the function returns [None], the evaluation
   can fail with a context error.
*)
val find_opt : BslKey.t -> bypass_map -> bypass option

(**
   For the typer, get the type of the bypass.
*)
val bypass_typer : bypass_map -> QmlTypes.bypass_typer

(**
   For the evaluator, this module is able to create directly
   a value of type [Value.t].

   The option is used because some bypass are defined,
   but do not have an implementation in ml available
   in the interpreter.

   In case this function returns [None], the eval loop
   can fail with a context error, telling that there is
   no available implementation of the bypass for opatop.
*)
val eval : bypass -> OpaTopValue.t option
