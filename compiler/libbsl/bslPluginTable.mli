(*
    Copyright © 2011, 2012 MLstate

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
   The imperative table where bsl pugins are stored.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(**
   Store a plugin in the table. Do not register any bypass yet anywhere,
   just put the plugin in the common table.
*)
val store : BslPluginInterface.plugin -> unit

(**
   Get a plugin from the common table by its name. None means a
   plugin with no name (i.e., a bundled plugin).
*)
val get : string option -> BslPluginInterface.plugin option

(**
   The finalization will check if the dependancies of plugins are satisfied.
   Example scenario : You have built a plugin [libB] using an other plugin [libA],
   with a command like :
   {[
   $ bslregister -o libB myfile.ml myfile2.ml libA.opp
   ]}
   And then, you try to compile an opa file which uses some primitives from libB :
   {[
   $ opa.exe libB.opp myfile.opa
   ]}
   Then, you'll get an error when you finalize the plugin table, because the plugin
   [libB] was built with a dependencie to the [libA], and you try to use
   the [libB] without loading the [libA].

   Add:
   {[
   $ opa.exe libB.opp libA.opp myfile.opa
   ]}

   Order of storing does not make any importance, the plugin are topologically sorted during the finalization,
   and then returned in a topologic order in the returned list.

   @error using standard OManager
*)
val finalize : unit -> BslPluginInterface.plugin list

(**
   Returns the last result returned by [finalize], but without refinalizing.
   If [finalize] was never called, returns [None]
*)
val last_finalize : unit -> BslPluginInterface.plugin list option

(**
   Clear
*)
val clear : unit -> unit
