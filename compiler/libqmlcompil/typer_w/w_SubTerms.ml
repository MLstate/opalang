(********************* LOCATE SUB TYPE******************************)
let int_to_string = function
  | 1 -> "first"
  | 2 -> "second"
  | 3 -> "third"
  | n -> string_of_int n ^ "rd"


let rec locate_subterms s t =
  match t with
   | W_Algebra.SType_var _ -> None
   | W_Algebra.SType_arrow(args, res) ->
      if (s == res.W_Algebra.sty_desc)
       then Some ([0], "result")
       else locate_subterms_in_args s args
   | _ -> None

(*check recursion!!!!!*)
and locate_subterms_in_args s args =
   match args with
   | [a] -> let a_desc = a.W_Algebra.sty_desc in
            if a_desc==s
              then Some ([1], "argument")
              else (match locate_subterms s a_desc with
                     | None -> None
                     | Some (n, str) -> Some (1::n, str ^ " of the argument")
                   )
   | _ -> (
   let rec aux n = function
      | [] -> None
      | a::args ->
        let a_desc = a.W_Algebra.sty_desc in
        if a_desc==s
         then Some ([n], int_to_string n ^ " argument")
         else match a_desc with
               | W_Algebra.SType_arrow(_, _) -> (
                 match (locate_subterms s a_desc) with
                  | Some(m, str) -> Some (n::m, str ^ " of the " ^ int_to_string n ^ " argument")
                  | _ -> aux (n+1) args
               )
               | _ -> aux (n+1) args
   in aux 1 args)

(********************** START SUBTERMS ******************************)
(*follow path to reconstruct args!!!!!!!*)
let __check_arrow_subterm n s = function
 | W_Algebra.SType_arrow(args, res) ->
   if n == 0
    then
     s == res.W_Algebra.sty_desc
    else if n < 0
    then false
    else
     s == (List.nth args (n-1)).W_Algebra.sty_desc
 | _ -> false


let rec get_subterm n = function
  | W_Algebra.SType_arrow(args, res) ->
    if n == 0
      then res.W_Algebra.sty_desc
      else (List.nth args (n-1)).W_Algebra.sty_desc
  | t -> t

let rec check_arrow_subterm n s t =
  let rec aux t = function
   | [] -> false
   | [i] -> __check_arrow_subterm i s t
   | j::i -> aux (get_subterm j t) i
 in aux t n

let rec take_subterms t =
  match t with
    | W_Algebra.SType_var _ -> [t]
    | W_Algebra.SType_sum_of_records ct -> t :: take_subterms_of_column_type ct
    | W_Algebra.SType_arrow (args, res) ->
       t :: List.concat (
             (take_subterms res.W_Algebra.sty_desc) ::
             (List.map (fun x -> take_subterms x.W_Algebra.sty_desc) args)
            )
    | W_Algebra.SType_forall t0 ->
       t :: take_subterms t0.W_Algebra.body.W_Algebra.sty_desc
    | W_Algebra.SType_named nt ->
       t :: List.concat (
         List.map take_subterms_simple_type nt.W_Algebra.nst_args
       )

and take_subterms_simple_type t = take_subterms t.W_Algebra.sty_desc

and take_subterms_of_row_type = function
  | {W_Algebra.rt_value = (ls, rte)} ->
      List.concat(
        (take_subterms_of_row_type_ending rte) ::
        List.map (fun (_, t) -> take_subterms t.W_Algebra.sty_desc) ls
      )
and take_subterms_of_row_type_ending = function
  | W_Algebra.Closed_row -> []
  | W_Algebra.Var_row rv -> take_subterms_of_row_variable rv

and take_subterms_of_row_variable rv =
  take_subterms_of_row_variable_value rv.W_Algebra.rv_value

and take_subterms_of_row_variable_value = function
  | W_Algebra.Row_unknown -> []
  | W_Algebra.Row_known rt -> take_subterms_of_row_type rt

and take_subterms_of_column_type = function
  | {W_Algebra.ct_value = (rt, cte)} ->
      List.concat(
        (take_subterms_of_column_type_ending cte) ::
        List.map take_subterms_of_row_type rt
      )

and take_subterms_of_column_type_ending = function
  | W_Algebra.Closed_column -> []
  | W_Algebra.Var_column cv -> take_subterms_of_column_variable cv

and take_subterms_of_column_variable cv =
   take_subterms_of_column_variable_value cv.W_Algebra.cv_value

and take_subterms_of_column_variable_value = function
  | W_Algebra.Col_unknown -> []
  | W_Algebra.Col_known ct -> take_subterms_of_column_type ct
