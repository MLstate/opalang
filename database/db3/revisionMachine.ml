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
module F = DbIo
module I = IoManager
module Db = Hldb
module DT = DbTypes

(* debug *)
#<Debugvar:DEBUG_DB$minlevel 1>

(* -- *)

let debug fmt =
  #<If> Printf.fprintf stdout ("[36m[RevisionMachine][0m"^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stderr ("[36m[RevisionMachine][0m"^^fmt^^"\n%!")
  #<End>

let print fmt = Logger.warning ("[RevisionMachine] "^^fmt)

let error ?(critical=false) fmt =
  if critical then
   Logger.critical ("[RevisionMachine] "^^fmt)
  else Logger.error ("[RevisionMachine] "^^fmt)

(* -- *)


let purge_eidmap lastrevision uidmap lasteid =
  let lasteid = Eid.succ lasteid in

  EidMap.fold
    (fun eid revisionmap neweidmap ->
      if Eid.compare eid lasteid = 1 then
        neweidmap
      else
        (let map =
           RevisionMap.fold
             (fun rev uid newrevisionmap ->
               if Revision.compare lastrevision rev = -1 then
                 newrevisionmap
               else RevisionMap.add rev uid newrevisionmap)
             revisionmap RevisionMap.empty in
         if RevisionMap.is_empty map then
           neweidmap
         else
           EidMap.add eid map neweidmap)
    ) uidmap EidMap.empty


let purge_nodemap lastuid nodemap =
  UidMap.filter_keys (fun uid -> Uid.compare lastuid uid <> -1) nodemap

let overwrite_files fm db =
  let uid = Db.get_next_uid db in
  let rev = Db.get_rev db in
  let uidmap = Db.get_uid_map db in
  let index = Db.get_index db in

  I.overwrite_flags ~uid ~rev fm;
  I.write_dbstate ~reset:false fm ~uidmap ~index;
  I.write_config_last_snapshot fm rev


let gotorevision_aux fm uidmap nodemap index rrevision =
  let uidrev = I.read_uid_rev ~rev:rrevision fm in

  if not (Revision.equal rrevision uidrev.DT.rev) then
    (error ~critical:true "Incoherent revisions : want to open at %s; found  %s"
           (Revision.to_string rrevision) (Revision.to_string uidrev.DT.rev);
     exit 3);

  debug "Reopen the db at revision %s: r %s e %s u %s"
    (Revision.to_string rrevision) (Revision.to_string uidrev.DT.rev) (Eid.to_string uidrev.DT.eid) (Uid.to_string uidrev.DT.uid);

  let newuidmap = purge_eidmap rrevision uidmap uidrev.DT.eid in

  let newnodemap = purge_nodemap (Option.get (Uid.pred uidrev.DT.uid)) nodemap in
  let new_db = Db.restart ~index rrevision uidrev.DT.eid uidrev.DT.uid newuidmap newnodemap in

  overwrite_files fm new_db;

  new_db

let gotorevision fm db =
  gotorevision_aux fm (Db.get_uid_map db) (Db.get_node_map db) (Db.get_index db)


(* Diagnostic & Restoration of db's files *)

(* Utils *)
let us = Uid.to_string
let es = Eid.to_string
let rs = Revision.to_string

let get_rev_id fm id compare field rev =
  let rec boucle rev =
    try
      let uidrev = I.read_uid_rev ~rev fm in
      let comp = compare (field uidrev) id in
      if comp = 1 then
        match Revision.pred rev with
        | Some rev -> boucle rev
        | None -> rev
      else uidrev.DT.rev
    with DT.CrashUidRev b ->
      if b then Revision.make 0
      else Option.default_map rev boucle (Revision.pred rev)
  in boucle rev

let get_rev_uid fm uid =
  get_rev_id fm uid Uid.compare (fun u -> u.DT.uid)

let get_rev_eid fm eid =
  get_rev_id fm eid Eid.compare (fun u -> u.DT.eid)

(* Diagnostic function *)
let restore_db ~restore ~uidrev fm =
  I.rebirth fm;

  let last_uid = uidrev.DT.uid
  and last_rev = uidrev.DT.rev
  and last_eid = uidrev.DT.eid in

  (* This buffer is for diagnostic result *)
  let buff = Buffer.create 100 in
  let addln fmt = Printf.bprintf buff ("\t"^^fmt^^"\n") in
  let add_emptyln () = Buffer.add_string buff "\n" in
  add_emptyln ();

  let nodemap, position_nodemap =
    try `FullNode (I.read_nodes fm), -1
    with DT.CrashNode (m,p) -> `PartialNode m, p in

  (* inreconstructible, faut voir ce que l'on a & faire avec *)
  let nodemap =
    (match nodemap with
    | `PartialNode m when UidMap.is_empty m ->
      addln "Node file: Empty"; m
    | `PartialNode m ->
      addln "Node file: broken until uid %s" (Uid.to_string (fst (UidMap.max m))); m
    | `FullNode m -> addln "Node file: OK"; m) in


  let transes, position_trans =
    try `FullTrans (I.read_transs fm), -1
    with DT.CrashTrans (tr,p) -> `PartialTrans tr, p in

  (* inreconstructible *)
  (match transes with
  | `FullTrans _ -> addln "Transaction file: OK"
  | `PartialTrans tr -> addln "Transaction file: broken, only %d transactions" (List.length tr));


  (* index recuperable partiellement
   * TODO *)
  let dbstate, position_dbstate =
    try `FullDbState (I.read_dbstate fm ), -1
    with DT.CrashStateMap (m,p) -> `PartialStateMap m, p
    | DT.CrashStateIndex (mi,p) -> `PartialStateIndex mi, p in

  let dbstate =
    (match dbstate with
    | `FullDbState m -> addln "DbState file: OK"; m
    | `PartialStateIndex m -> addln "DbState file: broken. All nodes but broken index (%d elements)" (StringMap.size m.DT.index); m
    | `PartialStateMap m ->
      addln "DbState file: broken. nodes until %s and no index" (Eid.to_string (fst (EidMap.max m)));
      {DT. uidmap = m; index = StringMap.empty})
  in

  let ffm = I.get_filemanager fm in

  let last_safe_revision =
    (* compute last safe revision *)
    let saferev =
      let uidrev =
        let max,_ = UidMap.max nodemap in
        if Uid.equal max last_uid then last_rev else get_rev_uid fm max last_rev in
      let eidrev =
        let max, _= EidMap.max dbstate.DT.uidmap in
        if Eid.equal max last_eid then last_rev else get_rev_eid fm max last_rev in
      let tmsrev =
        let pos =  DbIo.length ffm DbIo.Timestamp in
        let revpos = 9 * Revision.value last_rev in
        if pos = revpos then last_rev else Revision.make ( (pos / 9) -1 ) in
      debug "rev : %s uidrev : %s eidrev : %s tmsrev : %s" (rs last_rev) (rs uidrev) (rs eidrev) (rs tmsrev);
      List.hd (List.sort Revision.compare [ uidrev; eidrev; tmsrev ])
    in
    saferev
  in

  add_emptyln ();

  let res =
    if Revision.equal last_safe_revision (Revision.make 0) then
      (addln "Last safe revision is 0. The database can not be recovered.";
       None)
    else
      (addln "The database is coherent until revision %s" (rs last_safe_revision);

       if restore then
         (addln "I will restart at revision %s" (rs last_safe_revision);
          let location = #<If:TESTING> "" #<Else> " at '"^(I.get_location fm) ^ "'"#<End> in
          addln "A Backup was saved %s" location;
          I.do_backup fm;

          let db = gotorevision_aux fm dbstate.DT.uidmap nodemap dbstate.DT.index last_safe_revision in
          Some db)
       else
         (addln "To restart with a coherent database : ";
          addln "restart once with flags restore";
          addln "An automatic backup will be done";
          None);

      ) in

  (* keep this positions, can be useful *)
  ignore position_nodemap;
  ignore position_dbstate;
  ignore position_trans;


  print "%s" (Buffer.contents buff);
  res


let restore_uid_file ~restore fm =
  let last = I.restore_uidrev ~restore fm in

  (* This buffer is for diagnostic result *)
  let buff = Buffer.create 100 in
  let addln fmt = Printf.bprintf buff ("\t"^^fmt^^"\n") in
  let add_emptyln () = Buffer.add_string buff "\n" in
  add_emptyln ();

  let out () = print "%s" (Buffer.contents buff) in

  match last with
  | None ->
    addln "Last safe revision is 0. The database can not be recovered.";
    out ();
    None
  | Some last ->
    (addln "UidRev file: broken until revision %s" (Revision.to_string last.DT.rev);
     if restore then
       (out ();
        let _db : Db.t option = restore_db ~restore ~uidrev:last fm in
        Some last)
     else
       (addln "restart once with flag restore";
        addln "An automatic backup will be done";
        out ();
        None))
