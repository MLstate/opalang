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
/*(* The parser definition *)*/

%{
  open Requestdef
  open RequestType
  open Printf
%}

/*(* Tokens *)*/

/*(* Literals *)*/
%token <string> WORD
%token <string> SPACE

/*(* The end-of-file marker *)*/
%token EOL
%token EOF

/*(* Special symbols *)*/
%token COLON          /*(* ":" *)*/

/*(* Entry points *)*/
%start request
%type <RequestType.parse_request> request
%start response
%type <Requestdef.Value.value Requestdef.RequestHeader.t> response

%%

/*(* Requests *)*/

/*(*+request : {parse_request} <-
  / RequestLineShort{{Complete{request_line=__1; request_header=RequestHeader.empty; request_message_body=""; server_info=None}}}
  / RequestLine headers {{ Complete { request_line=__1; request_header=__2; request_message_body=""; server_info=None} }}
  / RequestLine {{ Incomplete }} *)*/
request:
    RequestLineShort
      { Complete { request_line=$1; request_header=RequestHeader.empty; request_message_body=""; server_info=None} }
  | RequestLine headers
      { Complete { request_line=$1; request_header=$2; request_message_body=""; server_info=None} }
  | RequestLine
      { Incomplete }
  ;

/*(*+response : {Requestdef.Value.value Requestdef.RequestHeader.t} <-
        / ResponseLine headers {{ __2 }}*)*/
response:
    ResponseLine headers { $2 }
  ;

/*(*RequestLine <- Word Space+ Word Space+ Word EOL
        {{  { _method=method_of_string __1; request_uri=__3; http_version=__5 } }}*)*/
RequestLine:
    WORD SPACE WORD SPACE WORD EOL
      { { _method=method_of_string $1; request_uri=$3; http_version=$5 } }
  ;

/*(* We need to accept HTTP/0.9 requests.*)*/
/*(*RequestLineShort <- Word Space+ Word EOL
        {{ { _method=method_of_string __1; request_uri=__3; http_version="HTTP/0.9" } }}*)*/
RequestLineShort:
    WORD SPACE WORD EOL
      { { _method=method_of_string $1; request_uri=$3; http_version="HTTP/0.9" } }
  ;

/*(*ResponseLine <- Word Space+ ([0-9]+ $_) (Space+ Word)+ EOL
        {{  __1, int_of_string(__3) }} *)*/
ResponseLine:
    WORD SPACE WORD Words EOL { ($1,(int_of_string $3)) }
  ;

Words:
    SPACE WORD Words { $2::$3 }
  | SPACE WORD { [$2] }
  ;

/*(*headers<-Header headers {{ let k,v=__1 in try RequestHeader.add (request_header_of_string k) v __2 with (Parsing _) -> __2 }}
        / EOL {{ RequestHeader.empty }}*)*/
headers:
    Header headers { let k, v=$1 in try RequestHeader.add (request_header_of_string k) v $2 with (Parsing _) -> $2 }
  | EOL { RequestHeader.empty }
  ;

/*(*Header <- Word [:] Space* field_value EOL {{ __1, __4 }}*)*/
Header:
    WORD COLON SPACE field_value EOL { ($1,`string $4) }
  ;

/*(*field_value <-
        (!EOL . {{ __2 }})* {{ `string (string_of_chars __1) }}*)*/
/*(*Really corny...*)*/
field_value:
    WORD field_value { $1^$2 }
  | COLON field_value { ":"^$2 }
  | SPACE field_value { $1^$2 }
  | WORD { $1 }
  | COLON { ":" }
  | SPACE { $1 }
  ;

%%
