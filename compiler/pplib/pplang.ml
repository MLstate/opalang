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
let ml_description = {
  Pprocess.open_com = "(* ";
  Pprocess.close_com = " *)";
  Pprocess.open_block = "begin ";
  Pprocess.close_block = "end";
  Pprocess.debug_module = "DebugVariables";
}

let opa_description = {
  Pprocess.open_com = "/*";
  Pprocess.close_com = "*/";
  Pprocess.open_block = "";
  Pprocess.close_block = "";
  Pprocess.debug_module = "TODO!!!";
}

let js_description = {
  Pprocess.open_com = "/*";
  Pprocess.close_com = "*/";
  Pprocess.open_block = "{";
  Pprocess.close_block = "}";
  Pprocess.debug_module = "TODO!!!";
}
