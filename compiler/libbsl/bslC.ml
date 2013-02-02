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
let prototypeC type_path_map current_path name args t =
  sprintf "%s %s(%s)" (BslTypes.to_c_coercion ~type_path_map ~current_path t) (String.concat_map "_" String.lowercase (current_path@[name])) (String.concat_map ~nil:"void" ", " (fun (name, typ) -> sprintf "%s %s" (BslTypes.to_c_coercion ~type_path_map ~current_path typ) name) args)

let ext = "c"
let lang = Language.of_string ext
let give_pointer = false
let string_of_directive ~type_path_map extra (_(*tags*), d) =
  let current_path = extra.ordered_path in
  let s1, s2_opt = match d with
    | ExternDef (name, _, _) -> sprintf "// ##extern-type %s" name, None
    | RecordDef (name, _, _) -> sprintf "// ##record %s" name, None
    | Module (bslkey, name, _) -> sprintf "// ##module %s \\ %s" bslkey name, None
    | Property -> "", None
    | EndModule -> "// ##endmodule", None
    | Register ((bslkey, s, _protected), ty) ->
        let ml = sprintf "// ##register %s \\ %s" bslkey s in
        let mli =
          let args, ret = BslTypes.TypeList.to_list ty in
          let args = let x = ref (-1) in List.map (fun ty -> incr(x); sprintf "x%d" !x, ty) args in
          let s = prototypeC type_path_map current_path s args ret in
          sprintf "extern %s ;" s
        in
        ml, Some mli
    | Args (name, args, t) ->
        let s = prototypeC type_path_map current_path name args t in
        s, Some "// args"
  in s1, default s1 s2_opt
let line_pointer filename = sprintf "#line 1 \"%s\"" filename
let impl_name_of_path_name ?runtime:_ _ name = name
let extra_header = "
/* representation of bsl-standard type */
typedef char unit; /* warning unit is different from {} in C libbsl (TODO: is it OK?) */
typedef char bool;
typedef void *ty_alphaval;
typedef void *ty_qmlval;
"
let extra_code = "
#define UNIT return(0)
"
let init_extra_code = extra_code, extra_header
let extra_static_checker _ _ = ()
let extra_static_validator _ _ _ = () (* no need an extra check with C : it is compiled with gcc *)
let module_dirtags () = None
let source_preprocess () = None
let introduction (buf_js, buf_jsi) =
  let buf_js = FBuffer.addln buf_js "/** Concatenation of all cbsl code */" in
  (buf_js, buf_jsi)
