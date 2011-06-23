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
(*
    @author Louis Gesbert
**)

module String = Base.String
module List = Base.List
module Hashtbl = Base.Hashtbl
module Dialog = Badop_lib.Dialog
module Dialog_aux = Badop_lib.Dialog_aux
let (@>) = Cps.Ops.(@>)
let (|>) = Cps.Ops.(|>)
let sprintf = Printf.sprintf

module F (Bk: Badop.S) =
struct

  type database = Bk.database

  type transaction_status = Fresh | Changed | Prepared | Committed | Failed

  type revision = Bk.revision

  type 'which read_op = ('which,revision) Badop.generic_read_op

  type ans = Badop.Dialog.response Bk.read_op Badop.answer

  type cache_entry =
    | CacheAnswer of (Dialog.query read_op * ans) list
    | CacheLink of Path.t

  type transaction = { (* Extended transaction (called xtr below) *)
    db: Bk.database;
    status: transaction_status;
    tr: Bk.transaction option;
    stash: (Badop.path * Dialog.query Bk.write_op) list;
    cache: (Badop.path, cache_entry) Hashtbl.t;
  }

  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  let open_database = Bk.open_database
  let close_database = Bk.close_database
  let status db k = Bk.status db @> fun st -> Badop.Layer("Cache", st) |> k

  let get_tr xtr k = match xtr.tr with
    | None -> Bk.Tr.start xtr.db @> k
    | Some tr -> tr |> k

  let flush xtr k = match xtr.stash with
    | [] -> xtr |> k
    | l ->
        get_tr xtr
        @> fun tr -> Bk.write_list tr (List.rev l)
        @> fun tr -> { xtr with tr = Some tr; status = Changed; stash = [] } |> k

  module Tr =
  struct

    let start db k =
      { db = db; tr = None; status = Fresh; stash = []; cache = Hashtbl.create 128; } |> k

    let start_at_revision db rev k =
      Bk.Tr.start_at_revision db rev
      @> fun tr -> { db = db; tr = Some tr; status = Fresh; stash = []; cache = Hashtbl.create 128; } |> k

    let prepare xtr k =
      flush xtr
      @> fun xtr ->
        match xtr.status with
        | Changed ->
            get_tr xtr
            @> fun tr -> Bk.Tr.prepare tr
            @> fun (tr,ok) -> ({ xtr with tr = Some tr; status = if ok then Prepared else Failed}, ok) |> k
        | Fresh | Prepared -> (xtr,true) |> k
        | Failed | Committed -> (xtr,false) |> k

    let rec commit xtr k =
      match xtr.status with
      | Prepared ->
          assert(xtr.stash = []);
          get_tr xtr @> fun tr -> Bk.Tr.commit tr @> k
      | Changed ->
             prepare xtr
          @> fun (xtr,ok) ->
            if ok
            then get_tr xtr
              @> fun tr -> Bk.Tr.commit tr
              @> k
            else false |> k
      | Fresh ->
          if xtr.stash = []
          then true |> k
          else flush xtr
            @> fun xtr -> commit xtr
            @> k
      | Committed -> true |> k
      | Failed -> false |> k

    let abort xtr k =
      match xtr.status with
      | Failed | Committed -> () |> k
      | _ -> match xtr.tr with Some tr -> Bk.Tr.abort tr @> k | None -> () |> k

  end

  (* For debug, we can get rid of this later... *)
  let string_of_DLU = function `Data -> "Data" | `Link -> "Link" | `Unset -> "Unset" | _ -> assert false
  let string_of_time t = Date.rfc1123 (Time.localtime t)
  let string_of_range (to_string:'a -> string) ((ao,i):'a Badop.range) = sprintf "(%s,%d)" (Option.to_string to_string ao) i
  let string_of_gro = function
    | Badop.Stat (Dialog.Query ()) -> "Query(Stat())"
    | Badop.Stat (Dialog.Response (path, rev_opt, _DLU)) ->
        sprintf "Response(Stat(%s,%s,%s))"
          (Path.to_string path) (Option.to_string Bk.Debug.revision_to_string rev_opt) (string_of_DLU _DLU)
    | Badop.Contents (Dialog.Query ()) -> "Query(Contents())"
    | Badop.Contents (Dialog.Response data) -> sprintf "Response(Contents(%s))" (DataImpl.to_string data)
    | Badop.Children (Dialog.Query key_range) -> sprintf "Query(Children(%s))" (string_of_range Keys.to_string key_range)
    | Badop.Children (Dialog.Response path_list) ->
        sprintf "Response(Children([%s]))" (String.concat_map "; " Path.to_string path_list)
    | Badop.Revisions (Dialog.Query rev_range) ->
        sprintf "Query(Children(%s))" (string_of_range Bk.Debug.revision_to_string rev_range)
    | Badop.Revisions (Dialog.Response rtl) ->
        sprintf "Response(Children([%s]))"
          (String.concat_map "; " (fun (r,t) -> sprintf "(%s,%s)" (Bk.Debug.revision_to_string r) (string_of_time t)) rtl)
    | Badop.Search (Dialog.Query (sl,ir)) ->
        sprintf "Query(Search([%s],%s))" (String.concat "; " sl) (string_of_range string_of_int ir)
    | Badop.Search (Dialog.Response kl) -> sprintf "Response(Search([%s]))" (String.concat_map "; " Keys.to_string kl)

  let really_read ans_list xtr path read_op k =
    flush xtr
    @> fun xtr -> get_tr xtr
    @> fun tr -> Badop.Aux.map_read_op ~revision:(fun r k -> r |> k) read_op
    @> fun bk_read_op -> Bk.read tr path bk_read_op
    @> fun ans ->
      #<If:BADOP_DEBUG$minlevel 10>Logger.debug "CACHING(%s,%s)" (Path.to_string path) (string_of_gro read_op)#<End>;
      Hashtbl.replace xtr.cache path (CacheAnswer ((read_op,ans)::ans_list));
      ans |> k

  let rec read xtr path read_op k =
    match Hashtbl.find_opt xtr.cache path with
    | Some (CacheAnswer ans_list) ->
        (match List.assoc_opt read_op ans_list with
         | Some ans ->
             #<If:BADOP_DEBUG$minlevel 10>Logger.debug "CACHED(%s,%s)" (Path.to_string path) (string_of_gro read_op)#<End>;
             ans |> k
         | None -> really_read ans_list xtr path read_op k)
    | Some (CacheLink p) ->
        #<If:BADOP_DEBUG$minlevel 10>Logger.debug "FOLLOWING(%s)" (Path.to_string p)#<End>;
        read xtr p read_op k
    | None -> really_read [] xtr path read_op k

  let write xtr path write_op k =
    (* We make some effort to update the read cache but mostly we just stomp on it *)
    (match write_op with
     | Badop.Set (Dialog.Query data) ->
         let gro = Badop.Contents (Dialog_aux.make_unsafe_response data) in
         #<If:BADOP_DEBUG$minlevel 10>Logger.debug "UPDATED(%s,%s)" (Path.to_string path) (string_of_gro gro)#<End>;
         Hashtbl.replace xtr.cache path (CacheAnswer[(Badop.Contents (Dialog_aux.make_unsafe_query ()),`Answer gro)
                                                     (* Can't add stat here because we can't predict the revision *)])
     | Badop.Clear (Dialog.Query ()) ->
         #<If:BADOP_DEBUG$minlevel 10>Logger.debug "CLEARED(%s)" (Path.to_string path)#<End>;
         Hashtbl.replace xtr.cache path (CacheAnswer [(Badop.Contents (Dialog_aux.make_unsafe_query ()),`Absent);
                                                      (Badop.Stat (Dialog_aux.make_unsafe_query ()),`Absent)])
     | Badop.Link (Dialog.Query p) ->
         #<If:BADOP_DEBUG$minlevel 10>Logger.debug "LINKED(%s->%s)" (Path.to_string path) (Path.to_string p)#<End>;
         Hashtbl.replace xtr.cache path (CacheLink p)
     (*| Badop.Copy (Dialog.Query _) ???*)
     | _ ->
         #<If:BADOP_DEBUG$minlevel 10>Logger.debug "INVALIDATED(%s)" (Path.to_string path)#<End>;
         Hashtbl.remove xtr.cache path);
    Badop.Aux.map_write_op ~transaction:(fun xtr k -> get_tr xtr @> k) ~revision:(fun r k -> r |> k) write_op
      (* only for types, no tr in queries *)
    @> fun bk_write_op ->
      Badop.Aux.respond_set_transaction write_op { xtr with stash = (path,bk_write_op)::xtr.stash }
      |> k

  let write_list xtr path_write_op_list k =
    let wr xtr (path, op) k =
      write xtr path op @> fun resp -> Badop.Aux.result_transaction resp |> k
    in
    Cps.List.fold wr xtr path_write_op_list k

  let node_properties db config k = Bk.node_properties db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
