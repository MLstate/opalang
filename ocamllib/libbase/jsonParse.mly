/*
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
*/
%{

 module J = JsonTypes

%}

%token EOF
%token LCURLY RCURLY COLON

%token LBRACKET RBRACKET COMMA
%token TRUE FALSE NIL
%token <string> STRING IDENT
%token <int> INT
%token <float> FLOAT

%start json

%type <JsonTypes.json> json

%%

json:
  | value { $1 }
  | error {
    let p = symbol_start_pos () in
    let msg = Printf.sprintf "line %d, char %d (%d) parse error\n"
	p.Lexing.pos_lnum p.Lexing.pos_bol p.Lexing.pos_cnum in
    failwith msg }

value:
  | STRING { J.String $1 }
  | INT { J.Int $1 }
  | FLOAT { J.Float $1 }
  | obj { $1 }
  | array { J.Array $1 }
  | TRUE { J.Bool true }
  | FALSE { J.Bool false }
  | NIL { J.Void }

obj:
  | LCURLY members RCURLY { J.Record (List.rev $2) }
  | LCURLY RCURLY { J.Void }

members:
  | pair { [$1] }
  | members COMMA pair { $3 :: $1 }

pair:
  | STRING COLON value { $1, $3 }
  | IDENT COLON value { $1, $3 }

array:
  | LBRACKET RBRACKET { [] }
  | LBRACKET elements RBRACKET { List.rev $2 }

elements:
  | elements COMMA value { $3 :: $1 }
  | value { [$1] }
