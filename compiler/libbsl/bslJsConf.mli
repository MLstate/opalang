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
   Configuration for jsbsl files transformations
   @author Mathieu Barbin
*)

(** {6 Purpose} *)
(**
   The js code from the bsl and plugins can also be transformed by passes
   working directly on javascript, such as :
   -cleanup (simplifications, constant folding, etc.)
   -local renaming (possibly with overlap)
   -local inline
   -etc.

   Nevertheless, some files are very hacky (example jquery), and contains
   specific workarround for contourning some hacks of some browser.
   A scary example is the following code, taken from jquery-1.4.4.:

   {[
   // Safari mis-reports the default selected property of an option
   // Accessing the parent's selectedIndex property fixes it
   if ( name === "selected" && !jQuery.support.optSelected ) {
     var parent = elem.parentNode;
     if ( parent ) {
       parent.selectedIndex;

       // Make sure that it also works with optgroups, see #5701
       if ( parent.parentNode ) {
         parent.parentNode.selectedIndex;
       }
     }
   }
   ]}

   The cleanup pass would simply remove the expressions [parent.selectedIndex] and [parent.parentNode.selectedIndex]
   because they does not make side effects, although the hack tell the contrary.
*)

(** {6 Configuration} *)
(**
   For activating only part of the passes, [bslregister] may take a [jsconf] file tagging some files
   for activating only part of the optimization done on files.

   Syntax:

   {[
   [myfile.js]
   cleanup: true
   localrenaming: false

   [myfile2.js, myfile3.js]
   cleanup: false
   localrenaming: true
   ]}

   It is possible to use the syntax [*] for representing all the js files,
   and to give inconsistent informations about property, in this case, the
   last given property.

   {[
   [*]
   verbatim:false

   [exception.js]
   verbatim:true
   ]}
*)

(**
   The type of the conf for activating part of the passes only
*)
type optimized_conf = {
  cleanup : bool ;
  localrenaming : bool ;
}

(**
   Verbatim means that the file is not parsed, and serialized as a verbatim string,
   without any check, and any optimization.
*)
type conf =
  | Verbatim
  | Optimized of optimized_conf

(**
   The default configuration.
*)
val default : conf

(**
   Tell if a conf is equal to the default configuration.
*)
val is_default : conf -> bool

(**
   The abstract type representing global js files configuration
*)
type t

(**
   The type for filename. Path for accessing the configuration file.
*)
type filename = string

(**
   Regrouping all the conf of all files, indexed by the basename of files, with the extension.
*)
type confs = conf StringMap.t

(**
   Export the confs from an API.
*)
val export : t -> confs

(**
   The empty confs. We need to know all the files, so that we can know what files are refered
   when the syntax [*] is used. The files contained in the set may be given with their basename,
   with the extension.
   The basename should be the name used in the configuration files.
   These basename will be reused as index of the exported [confs].
   Reading a conf file, if a file is not in the list, this will be a warning.
*)
val empty : string list -> t

(**
   Fold confs using configuration files
*)
val fold : filename -> t -> t
