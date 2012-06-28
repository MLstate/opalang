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

(* depends*)
module List = BaseList

(* shorthands *)
module F = Init.F
module Migration = Init.Migration

#<Debugvar:DEBUG_DB$flag "migration">

(* -- *)

exception Migration_error

(* How to add a migration function
 * exemple : migrate to version 42
 * Create the file 'migration_42.ml'
 * ø write in the function :
 *   signature : val migrate_to_version_42 : FileIo.DbIo.t -> unit
 * ø use Init.Migration module for initialising & finalize file to change
 * ø explain changes
 * ø add it to table_update (keep the order, please)
 * ø add the file in the database.mllib
 * That's it !!
 *
 * There is a canevas (the last entry of the list)
 * Don't forget to create another canevas (your migration + 1)
 * and to update reftester test : db-shell (04)
 *)

let table_update =
  [
    (18, Migration_18.migrate_to_version_18);
    (19, Migration_19.migrate_to_version_19);
    (20, Migration_20.migrate_to_version_20);
    (21, Migration_21.migrate_to_version_21);
    (22, Migration_22.migrate_to_version_22);
    (23, Migration_23.migrate_to_version_23);
    (24, Migration_24.migrate_to_version_24);
    (25, Migration_25.migrate_to_version_25);
    (26, Migration_26.migrate_to_version_26);
  ]


let update version current_version fm =
  Init.debug ( "upate version\n" ^^
                          "  | Version of the exe : %d\n"^^
                          "  | Version of the physical database : %d\n"^^
                          "  | Availabale migration : %s"
                      )
    current_version version (List.print (fun (x,_) -> string_of_int x) table_update);
  if version > current_version then (
    Init.error (
      "This database is more recent than the executable you are using. Giving up.\n"^^
        " | Version of the exe : %d\n"^^
        " | Version of the db  : %d\n"^^
        "Retro-migrations are not supported, please upgrade your executable."
    )
      current_version
      version
    ;
    raise Migration_error
  ) ;
  let location = F.get_location fm in
  let lst = List.filter (fun (x, _) -> x > version && x <= current_version) table_update in
  if lst = [] then (
    Init.error "This database was created by an incompatible binary. No update found. Giving up.";
    raise Migration_error
  );
  (* FIXME on suppose qu'elle est sorte? *)
  let lst = List.sort (fun (x,_) (a,_) ->  compare x a) lst in
  let last_version = List.fold_left
    (fun prev (v, f) ->
       if v <> prev + 1 then (
         Init.error "Can not found migration for version %d to %d, next %d." prev (prev+1) v;
         raise Migration_error
       )
       else (
         Init.print "Begin migration to version %d%s" v (#<If:TESTING> "" #<Else> " of db "^location #<End>);
         try
           let version = Migration.make_version v in
           let (_ : Migration.final) = f version fm in
           Init.print "Migrate succesfully to version %d" v
         with _e -> (
           Init.error "Problem during migration, abort";
           #<If>
             let cause = Printexc.to_string _e in
             let bt = Printexc.get_backtrace() in
             Init.error "Cause : %s" cause;
             Init.error "Backtrace : %s" bt
               #<End>;
             raise Migration_error
         )
       );
       v) version lst in
  if last_version <> current_version then
    (Init.error "Database '%s' is at version %d; current is %d" location last_version current_version; raise Migration_error)
