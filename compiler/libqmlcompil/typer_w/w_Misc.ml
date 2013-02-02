let cmp_pos p1 p2 =
  let (f1, l1) = FilePos.get_one_loc p1 in
  let (f2, l2) = FilePos.get_one_loc p2 in
  if f1 = f2 then compare l1 l2 else (-2)

let error_position = ref (FilePos.nopos "unitialized")

let set_error_position p =
  error_position := p

let to_string_range_short fe r =
  let start = r.FilePos.start in
  let stop = r.FilePos.stop in
  try(
    let (line1, col1) = FilePos.get_pos fe start in
    let (line2, col2) = FilePos.get_pos fe stop in
    if FilePos.get_file (!error_position) = fe
     then (true, Printf.sprintf "(%d:%d-%d:%d)" line1 col1 line2 col2)
     else (false, Printf.sprintf "%s:(%d:%d-%d:%d)\t" fe line1 col1 line2 col2)
    )
  with
  | Not_found ->
      (false, Printf.sprintf "%s: (%d-%d)\t" fe start stop)

let pp_filerange_short filename fmt r =
  let (flag, str) = to_string_range_short filename r in
  if flag
   then Format.fprintf fmt "%s" (str ^ String.make (20 - String.length str) ' ')
   else Format.pp_print_string fmt str

let pp_filerange_short fmt {FilePos.filename=filename; FilePos.ranges=ranges} =
  let ranges = HdList.unwrap ranges in
  BaseFormat.pp_list "@\n" (pp_filerange_short filename) fmt ranges

let pp_pos_short fmt = function
  | FilePos.Builtin pass -> Format.fprintf fmt "<no position available (%s)>" pass
  | FilePos.Files (files, _) ->
      let files = HdList.unwrap files in
      BaseFormat.pp_list "@\n" pp_filerange_short fmt files
