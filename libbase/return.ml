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
type 'a label = 'a ref * exn

let return (r,e) value =
  r := value;
  raise e

let set_checkpoint_either f =
  let r = ref (Obj.magic 0) in
  let module M = struct exception Return end in
  (* defining a local exception, it is different from any other one *)
  try
    Base.Left (f (r,M.Return))
  with
  | M.Return -> Base.Right !r

let set_checkpoint_opt f =
  match set_checkpoint_either f with
  | Base.Left () -> None
  | Base.Right a -> Some a

let set_checkpoint f =
  match set_checkpoint_either f with
  | Base.Left a -> a
  | Base.Right a -> a

let set_checkpoint_none f =
  match set_checkpoint_either f with
  | Base.Left a -> Some a
  | Base.Right () -> None
