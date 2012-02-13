/*
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
