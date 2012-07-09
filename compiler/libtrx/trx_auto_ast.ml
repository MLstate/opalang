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
(*
  @author François-Régis Sinot
*)

(* =========================================================================================================== *)
(* ========================================= Utilities for --auto-ast ======================================== *)
(* =========================================================================================================== *)

open Trx_runtime

type auto_ast =
    [ `Rule of pos * pos * string * string option * auto_ast
    | `Seq of pos * pos * auto_ast list
    | `Choice of auto_ast
    | `Literal of pos * string
    | `Class of char
    | `Option of auto_ast option
    | `Star of auto_ast list
    | `Plus of auto_ast list
    | `And
    | `Not
    ]

module Subs : TraverseInterface.S with type 'a t = auto_ast constraint 'a = 'b * 'c * 'd =
struct
  type 'a t = auto_ast constraint 'a = 'b * 'c * 'd

  let subs_cons e =
    let fun0 e = function [] -> e | _ -> assert false in
    let fun1 f = function [x] -> f x | _ -> assert false in
    match e with
    | (`Option None | `And | `Not | `Class _ | `Literal _)-> fun0 e, []
    | `Rule (p1, p2, name, full, r) -> fun1 (fun r -> `Rule (p1, p2, name, full, r)), [r]
    | `Choice r -> fun1 (fun r -> `Choice r), [r]
    | `Option (Some r) -> fun1 (fun r -> `Option (Some r)), [r]
    | `Star l -> (fun l -> `Star l), l
    | `Plus l -> (fun l -> `Plus l), l
    | `Seq (p1, p2, l) -> (fun l -> `Seq (p1, p2, l)), l
end

module Walk = Traverse.Make (Subs)

let rec fold_rule (f : 'a -> pos * pos * string * string option -> 'a) = 
  Walk.fold (fun acc -> function
    | `Rule (p1, p2, n, n_opt, _rest) -> f acc (p1, p2, n, n_opt)
    | _ -> acc)


  
