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
