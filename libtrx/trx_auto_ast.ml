(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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


  
