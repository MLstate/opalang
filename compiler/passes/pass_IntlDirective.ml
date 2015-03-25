(**
 * Copyright © 2015 MLstate
 *
 * This file is part of Opa.
 *
 * Opa is free software: you can redistribute it and/or modify it under the
 * terms of the GNU Affero General Public License, version 3, as published by
 * the Free Software Foundation.
 *
 * Opa is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Opa. If not, see <http://www.gnu.org/licenses/>.
 *)

(** @author Henri Chataing *)

module SA = SurfaceAst
module SAP = SurfaceAstPassesTypes
module SAC = SurfaceAstCons.StringCons
module Intl = OpaEnv.Intl

module StringMap = Map.Make (String)

(** Generate fresh replacement keys. *)
let refId = ref (-1)
let genId () =
  incr refId;
  !refId
let resetId () =
  refId := -1

(**
 * Compute a hash of the expression (if the expression is simple and not too
 * large. Function applications should be executed only once, and we must be
 * careful not to override this rule.
 *)
let rec hash e lvl =
  if lvl > 3 then None (* The limit is set at expressions of depth 3, for now. *)
  else
    match fst e with
    | SA.Apply (f, args) ->
      let args = (SA.Record (fst args), snd args) in
      begin match (hash f (lvl+1)), (hash args (lvl+1)) with
      | Some (hf, _), Some (hargs, _) -> Some (hf^hargs, true)
      | _ -> None
      end
    | SA.Const (SA.CInt i) -> Some (Big_int.string_of_big_int i, false)
    | SA.Const (SA.CString s) -> Some ("\""^s^"\"", false)
    | SA.Const (SA.CFloat f) -> Some (string_of_float f, false)
    | SA.Ident id -> Some (id, false)
    | SA.Record fields ->
      let h = List.fold_left (fun h (name, value) ->
        match h with
        | Some (h, apps) ->
          begin match hash value (lvl+1) with
          | Some (hv, appsv) -> Some (h^name^":"^hv^",", apps || appsv)
          | None -> None
          end
        | None -> None
      ) (Some ("{", false)) fields in
      begin match h with
      | Some (h, apps) -> Some (h^"}", apps)
      | None -> None
      end
    | SA.Dot (record, field) ->
      begin match hash record (lvl+1) with
      | Some (hc, apps) -> Some (hc^"."^field, apps)
      | None -> None
      end
     (* Complex expressions are all ignored. *)
    | SA.ExtendRecord _| SA.Lambda _ | SA.LetIn _ | SA.Match _ | SA.DBPath _
    | SA.Bypass _ | SA.Directive _ -> None

(**
 * Extract the format from an option constant expression, and return the
 * associated type hint. The expression should be stringified only if the
 * format cannot be identified or does not correspond to a specific type.
 * TODO: build types.
 *)
let formatType typ =
  match typ with
  | Some typ -> ","^typ
  | None -> ""

(** Just extract the format style, returning an empty string if not found. *)
let formatStyle style =
  match style with
  | Some style -> ","^style
  | None -> ""

(** Just extract the format offset, returning an empty string if not found. *)
let formatOffset offset =
  match offset with
  | Some offset -> "offset:"^(string_of_int offset)
  | None -> ""

let formatSelector sel =
  match sel with
  | SA.IcuKeyword keyword -> keyword
  | SA.IcuCase n -> "="^(string_of_int n)

(**
 * If the key is a simple expression, check if it has already been assigned to
 * an integer identifier. If not, a new identifier is created and the key added
 * to the map. The return value is always the id and the updated map.
 *)
let storeKey key typ context mem =
  match hash key 0 with
  | Some (h, _) ->
    begin try
      (StringMap.find h mem, context, mem)
    with Not_found ->
      let id = genId () in
      (id, IntMap.add id (typ, key) context, StringMap.add h id mem)
    end
  | None ->
    let id = genId () in
    (id, IntMap.add id (typ, key) context, mem)

(**
 * Stringify an argument.  NB: we must add a bypass for the strringification
 * of dates, as the default stringifier returns pretty printed values.
 *)
let stringify (typ, e) =
  let _, label = e in
  match typ with
  | Some "time" | Some "date" ->
    let rawdate = SAC.E.ident ~label:label Opacapi.Date.raw in
    SAC.E.apply ~label:label rawdate e
  | Some _ | None ->
    let e = SA.Directive (`magic_to_string, [e], []), label in
    SAC.D.string ~label:label [e] (* Force the stringification of the argument. *)

(** Convert the message context to a list. *)
let buildContext context label =
  (* Context as ocaml list. *)
  let context = IntMap.fold (fun _ e l ->
    (stringify e)::l
  ) context [] in
  (* Context as Opa list. *)
  List.fold_left (fun l e ->
    SAC.E.cons ~label:label e l
  ) (SAC.E.nil ~label:label ()) context

(**
 * The call to intl. Two function applications occur: construction of the printer, application
 * of the printer to the context.
 *)
let buildIntl message context label =
  let msg = SAC.E.string ~label:label message in
  let intlformat = SAC.E.ident ~label:label Opacapi.Intl.format in
  let locale = SAC.E.applys ~label:label (SAC.E.ident ~label:label Opacapi.Intl.locale) [] in
  let intlprinter = SAC.E.ident ~label:label Opacapi.Intl.printer in
  let printer = SAC.E.apply2 ~label:label intlprinter msg locale in
  (* Apply the newly constructor printer to the formatter. *)
  SAC.E.apply2 ~label:label intlformat printer context

(**
 * Extract the string inserts, while replacing them by normalized variable names (e.g. k0, k1 ..).
 * TODO use a string builder for the generation of the string format.
 *
 * @param e an ICU message part.
 * @param context context of the message format.
 * @param buf reconstructed message format.
 * @return the normalized string format, and the context containg the variables associated with
 *  proposed types (e.g. if the insert type is date, the inserted expression must be typed 'Date.date').
 *)
let rec extractInserts e context mem buf =
  match e with
  (* Raw string part => no expressions to extract. *)
  | SA.IcuText s ->
    Buffer.add_string buf s;
    context, mem

  (* Simple formatting. *)
  | SA.IcuSimple (key, typ, style) ->
    let id, context, mem = storeKey key typ context mem in
    Buffer.add_string buf "{k";
    Buffer.add_string buf (string_of_int id);
    Buffer.add_string buf (formatType typ);
    Buffer.add_string buf (formatStyle style);
    Buffer.add_char buf '}';
    context, mem

  (* Plural switch. *)
  | SA.IcuPlural (key, offset, cases) ->
    let id, context, mem = storeKey key (Some "int") context mem in
    Buffer.add_string buf "{k";
    Buffer.add_string buf (string_of_int id);
    Buffer.add_string buf ",plural,";
    Buffer.add_string buf (formatOffset offset);
    (* Append all cases. *)
    let context, mem = List.fold_left (fun (context, mem) (sel, case) ->
      let selector = formatSelector sel in
      Buffer.add_string buf selector;
      Buffer.add_char buf '{';
      let context, mem = extractAllInserts case context mem buf in
      Buffer.add_char buf '}';
      context, mem
    ) (context, mem) cases in
    (* Close braces and return context. *)
    Buffer.add_char buf '}';
    context, mem

  (* Select switch. *)
  | SA.IcuSelect (key, cases) ->
    let id, context, mem = storeKey key None context mem in
    Buffer.add_string buf "{k";
    Buffer.add_string buf (string_of_int id);
    Buffer.add_string buf ",select,";
    (* Append all cases. *)
    let context, mem = List.fold_left (fun (context, mem) (selector, case) ->
      Buffer.add_string buf selector;
      Buffer.add_char buf '{';
      let context, mem = extractAllInserts case context mem buf in
      Buffer.add_char buf '}';
      context, mem
    ) (context, mem) cases in
    (* Close braces and return context. *)
    Buffer.add_char buf '}';
    context, mem

(** Same as extractInserts, applied to an @intl directive. *)
and extractAllInserts msg context mem buf =
  List.fold_left (fun (context, mem) part ->
    extractInserts part context mem buf
  ) (context, mem) msg

(**
 * Transform an @intl endpoint. The transformation takes place in different steps:
 *  1. Extraction and aliasing of inserted expressions.
 *  2. Generation of the ICU string format used by Intl.
 *  3. Generation of IntlMessageFormat function call to build the formatter.
 *  4. Construction of the formatting context out of the extracted expressions.
 *  5. Generation of the call to IntlMessageFormat.format, using the string format and the context.
 *
 * @param e an @intl directive
 * @return the transformed directive along with the string format.
 *)
let explicitIntl e label =
  resetId ();
  let buf = Buffer.create 32 in
  let context, _ = extractAllInserts e IntMap.empty StringMap.empty buf in
  let format = Buffer.contents buf in
  let context = buildContext context label in
  buildIntl format context label


(** Replace the calls to the directive @locale by calls to the API method Intl_locale. *)
let explicitLocale label =
  SAC.E.applys ~label:label (SAC.E.ident ~label:label Opacapi.Intl.locale) []


(** Replace all @intl directives. *)
let replaceIntlDirectives env =
  let replaceIntl code =
    OpaWalk.Code.map_down (fun ((e, label) as v) ->
      match e with
      | SA.Directive (`intl [], _, _) -> SA.Const (SA.CString ""), label
      | SA.Directive (`intl icu, _, _) -> explicitIntl icu label
      | SA.Directive (`locale, _, _) -> explicitLocale label
      | _ -> v
    ) code in
  let lcodeNotUser = replaceIntl env.SAP.lcodeNotUser in
  let lcodeUser    = replaceIntl env.SAP.lcodeUser    in
  { env with SAP.
    lcodeNotUser = lcodeNotUser;
    lcodeUser    = lcodeUser }

(** Apply replaceIntlDirectives as a surface ast pass. *)
let process ~options env =
  let _locales = options.OpaEnv.intl.Intl.locales in
  replaceIntlDirectives env
