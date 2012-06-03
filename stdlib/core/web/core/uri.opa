/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * @author Adam Koprowski, February 2010
 * @author David Rajchenbach-Teller (packaging), December 2010
 *
 * @destination public
 */

import stdlib.core.{parser}

// FIXME, this module needs a clean-up

/**
 * {1 About this module}
 *
 * This module contains a datatype for representation of URIs/URLs.
 *
 *
 * {1 Where should I start?}
 *
 * See type {!Uri.uri} for a datatype representing an URI. See module
 * {!Uri} for some functions for manipulating URIs.
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * This type represents user credentials that can optionally be put in an URI
 * and consists of an (optional) [username] and an (optional) [password].
 */
type Uri.uri_credentials = { username : option(string)
                           ; password : option(string)
                           }
/**
 * Type representing an URI (Uniform Resource Identifier).
 * The general form of an URI is:
 * {v foo://username:password@example.com:8042/over/there/index.dtb?type=animal;name=ferret#nose v}
 * This type contains the following fields (in parentheses we provide the relevant
 * part in the above example:
 *  - an optional [schema], ex. 'http', 'https', 'mailto' ({v foo v}),
 *  - optional user [credentials]; see [Uri.uri_credentials] ({v username:password v}),
 *  - domain ({v example.com v}),
 *  - optional port number ({v 8042 v}),
 *  - path, as a list of sequences separated by a forward slash ("/") ({v /over/there/index.dtb v}),
 *  - optional query string in the form of the list {v key=value v} separated by semicolon (";") or
 *    ampersand ("&") ({v type=animal;name=ferret v}),
 *  - and optional fragment ({v nose v}).
 *
 */
type Uri.uri = Uri.absolute / Uri.relative / Uri.mailto

type Uri.absolute =
     /**Absolute URI*/
                { schema : option(string) // FIXME enumeration instead of string?
                ; credentials : Uri.uri_credentials
                ; domain : string
                ; port : option(int)
                ; path : list(string)
                ; query : list((string,string))
                ; fragment : option(string)
                ; is_directory : bool
                }

type Uri.relative =
     /**Relative URI*/
                { path : list(string)
                ; fragment : option(string)
                ; query : list((string, string))
                ; is_directory : bool
                ; is_from_root : bool
                }

type Uri.mailto =
     /**mailto URI*/
                { address : string
                ; query : list((string, string))
                }

/**
 * For most purposes we treat URLs as synonyms to URIs, so [Url.url] is a synonym of {!Uri.uri}.
 */
type Url.url = Uri.uri

/**
 * {1 Parsing rules for URIs}
 */

/**
 * A module with parsing rules for URIs.
 */
UriParser =
{{

  /**
   * {2 High-level API}
   */

   /* TODO: this should do in most cases; however for full conformance with
      specification, confront this with RFC 3986 */

  /**
   * A parser for URIs.
   * Parser follows URI format, as outlined  at:
   * {{:http://en.wikipedia.org/wiki/URI_scheme#Generic_syntax} URI scheme: Generic syntax (Wikipedia)}.
   * Also see {!Uri.uri} for details on the accepted format of URIs.
   */
  uri = parser
    | Rule.ws ~schema res={
        match schema with
        | {http} | {https} | {ftp}-> http_uri(some(schema2string(schema)))
        | {mailto} -> mailto_uri
      } -> res
    | Rule.ws ~path ~query? ~fragment? Rule.ws ->
      {path=path.path ~fragment query=query ? []
       is_from_root=path.is_from_root
       is_directory=path.is_directory
      }

  http_uri(schema) = parser
    credentials=authority? ~domain ~port? path={parser "/" p=path -> p}?
    ~query? ~fragment? Rule.ws ->
       { ~schema
       ; credentials=credentials ? {username=none password=none}
       ; ~domain
       ; ~port
       ; path=Option.map(_.path, path) ? []
       ; is_directory=Option.map(_.is_directory, path) ? false
       ; query=query ? []
       ; ~fragment
       } : Uri.uri

  mailto_uri =
    parser address=chars_mailto query=query? -> { ~address query=query ? [] }

  /**
   * {2 Low-level API}
   */

   /**
    * RFC 2396 http://www.ietf.org/rfc/rfc2396.txt
    * The angle-bracket "<" and ">" and double-quote (") characters are
    * excluded because they are often used as the delimiters around URI in
    * text documents and protocol fields.  The character "#" is excluded
    * because it is used to delimit a URI from a fragment identifier in URI
    * references (Section 4). The percent character "%" is excluded because
    * it is used for the encoding of escaped characters.
    *    delims = "<" | ">" | "#" | "%" | <">
    *
    * Other characters are excluded because gateways and other transport
    * agents are known to sometimes modify such characters, or they are
    * used as delimiters.
    *    unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
    */

  unwise = parser t=([{}|\\^\[\]`"]+) -> Text.to_string(t); //"

  reserved = parser t=([;/?:@&=+$,]+) -> Text.to_string(t);

  mark = parser t=([\-_.!~*'()]+) -> Text.to_string(t);

  escaped = parser "%" h1=Rule.hexadecimal h2=Rule.hexadecimal ->
        String.of_utf8_val(h1 * 16 + h2)

  unreserved = parser t=((Rule.alphanum_char | mark)+) -> Text.to_string(t);

  uric = parser a=reserved -> a | a=unreserved -> a | a=escaped -> a;
  chars = parser v=uric+ -> String.flatten(v)

  uric_query = parser a=unreserved -> a | a=escaped -> a | [+] -> " " | t=([/:@+$, ]) -> Text.to_string(t) | a=unwise -> a;
  chars_query = parser v=uric_query+ -> String.flatten(v)
  opt_chars_query = parser v=uric_query* -> String.flatten(v)

  uric_path = parser a=unreserved -> a | a=escaped -> a | " " -> " ";
  chars_path = parser v=uric_path+ -> String.flatten(v)

  uric_mailto = parser a=uric -> a | t=("@") -> Text.to_string(t);
  chars_mailto = parser v=uric_mailto+ -> String.flatten(v)

  schema2string =
    | {mailto} -> "mailto"
    | {http} -> "http"
    | {https} -> "https"
    | {ftp} -> "ftp"

  schema =
    // TODO do we want to include other schemas here?
    schema_name = parser
    | "ftp" -> {ftp}
    | "https" -> {https}
    | "http" -> {http}
    | "mailto" -> {mailto}
    parser schema=schema_name ":" "//"? -> schema

  authority =
    pwd_parser = parser ":" pwd=Rule.alphanum_string -> pwd
    parser login=Rule.alphanum_string pwd=pwd_parser? "@" -> {username=some(login) ; password=pwd}

  port = parser ":" portno=Rule.natural -> portno

  path =
     // apart from alphanumerical characters we also allow tilde in path segments
     // TODO This string/text distinction in parsers is just not good...
    path_chars = parser v=chars_path -> v
    path_content = Rule.parse_list_sep(false, path_chars, parser [/])
    have_slash = parser r=([/]+)? -> Option.is_some(r)
    parser
       is_from_root=have_slash v=path_content is_directory=have_slash ->
       f(item, acc) =
          match item with
          | "" | "." -> acc //Eliminate noop
          | ".." -> match acc with
             | []     -> [] //Don't climb higher than root
             | [_|tl] -> tl
           end
          | _ -> [item|acc]
       end
       ~{is_from_root is_directory
         path = List.rev(List.fold(f, v, []))}

   /**
    * Domain names are restricted to the 26 characters in the English language, plus numbers.
    * The only "special" characters outside the basic alpha-numeric sets are hyphens.
    * You may use hyphens in a domain name as a method of separating words.
    * Domain names can optionally comprise entirely of letters or numbers.
    * They must not begin or end with a hyphen.
    * One restriction applied to domain names is a limit of 63 characters before the "TLD extension".
    * TLD extensions are the ".com", ".net", or country code like ".com.au" part of the domain name.
    * The "http://www." part of the domain name is not included in the character count.
    * Domain names are also case-insensitive.
    * Upper-case, lower-case, and mixed-case domain names all point to the one site.
    * While it is conventional to display names in lower-case, mixed-case can highlight word separation.
    * Eg. JohnSimmonsBikes.com
    *
    * (from http://www.servergrade.com.au/faq/answers/domain-letters-numbers.html)
    */
  domain =
     /* domain segment is a sequence of alphanumerical characters,
        possibly separated by hyphens (-) (but no hyphen at the
        beginning nor end of the segment is allowed) */
    domain_segment =
      dsp = Rule.parse_list_non_empty(Rule.alphanum_string, parser [\-])
      parser ls=dsp -> List.to_string_using("", "", "-", ls)
     /* domain is a list of domain segments (1 or more) separated with dots (.) */
    segs = Rule.parse_list_non_empty(domain_segment, parser [.])
    parser ls=segs -> List.to_string_using("", "", ".", ls)

  query_element = parser key=chars_query value=("=" value=opt_chars_query -> value)? -> (key, value ? "")
  query_parser = Rule.parse_list(query_element, parser [&;] -> void)

  query =
    parser "?" "&"? query=query_parser -> query

  fragment = parser
    | "#" fragment=chars -> fragment
    | "#" -> "" // fragment can be empty

}}

/**
 * {1 A module with functions on URIs}
 */
Uri =
{{

  /**
   * A parser for URIs
   */
  uri_parser = UriParser.uri

  of_absolute(absolute: Uri.absolute): Uri.uri =
    @opensums(absolute)

  of_relative(relative: Uri.relative): Uri.uri =
    @opensums(relative)

  default_absolute: Uri.absolute = {schema = {none} credentials = {username = {none} password = {none}} domain="example.org" port = {none} path=[] query=[] fragment = {none} is_directory=true}

  default_relative: Uri.relative =
    { path=[] fragment=none query=[] is_directory=false is_from_root=true }

  default_absolute_domain(domain,path) =
   {default_absolute
      with ~domain ~path}

  /**
   * Convert a string into a URI.
   *
   * @param string for conversion.
   * @return optional URI represented by the given string.
   */
  of_string = Parser.try_parse(uri_parser, _)

  /**
   * Encode a string meant to be injected in a URI to ensure that it does not contain any reserved character
   */
  encode_string: string -> string = %% BslString.encode_uri_component %%

  /**
   * Decode a string injected in a URI
   * Reverse function of [encode_string]
   */
  decode_string: string -> string = %% BslString.decode_uri_component %%

  /**
   * Decide whether a string represents a well-formed and secure URI.
   *
   * @param s A string
   * @return true if the string represents a valid [http], [https], [ftp] or [mailto] URI.
   */
  // FIXME, secure? in what sense secure?
  is_secure(s:string) =
     match of_string(s) with
        | {none} -> false
        | _      -> true

  /* FIXME, the distinction between relative/absolute URIs is not too good; improve?*/
  is_absolute(uri : Uri.uri) : bool =
    match uri with
    | ~{schema=_ domain=_ path=_ ...} -> true
    | _ -> false

  /**
   * Returns true for a valid, non-local HTTP(s) address.
   *
   * @param uri an URI
   * @return true iff [uri] is a valid HTTP(s) address (syntactically; it doesn't
   *         mean that such a page exists/is up and running).
   */
  is_valid_http(uri : Uri.uri) : bool =
    path_contains_a_dot =
      | [hd|_] -> String.index(".", hd) |> Option.is_some
      | _ -> false
    match uri with
    /* absolute URIs are OK if they have HTTP/HTTPs schemas or at least one
       dot in the path ([localhost] is not good, [test.com] is) */
    | ~{schema path ...} ->
        (match schema : option(string) with
        | {some="http"} -> true
        | {some="https"} -> true
        | {some=_} -> false
        | {none} -> path_contains_a_dot(path)
        )
    /* relative URIs are OK if they have a dot in the path (as above) */
    | ~{path ...} -> path_contains_a_dot(path)
    /* email addresses are not ok */
    | {address=_ query=_} -> false


  encode_path = List.map(encode_string, _)

  path_to_string(path) = String.concat("/", encode_path(path))

  /**
   * Conversion from URIs to their string representation.
   *
   * @param u an URI to be converted
   * @return a string representation of [u].
   */
  @stringifier(Uri.uri) to_string(u : Uri.uri) =
    query_to_string(q) =
      match q : list with
      | [] -> ""
      | _ -> "?" ^ String.of_list((v1, v2) -> "{encode_string(v1)}={encode_string(v2)}", "&", q)
    match u with
      | ~{path query fragment is_from_root is_directory} ->
           beg_path = if is_from_root then "/" else ""
           path_string = path_to_string(path)
           end_path = if is_directory then "/" else ""
           frag_string = match fragment with ~{some} -> "#{some}" | {none} -> ""
           query_string= query_to_string(query)
           "{beg_path}{path_string}{end_path}{frag_string}{query_string}"
      | ~{schema credentials domain port path query fragment is_directory} ->
           schema_string=Option.switch(schema -> "{schema}://", "", schema)
           cred_string  =
              Option.switch(
                 usr -> Option.switch(pwd -> "{usr}:{pwd}@", "{usr}", credentials.password),
                 "",
                  credentials.username)
           domain_string= domain
           port_string  = Option.switch(port -> ":{port}", "", port)
           path_string  = match path with [] -> "/"
                          | _ -> path_to_string([""| path]) end
           end_path     = if is_directory then "/" else ""
           query_string = query_to_string(query)
           frag_string  = Option.switch(frag -> "#{frag}", "", fragment)
           "{schema_string}{cred_string}{domain_string}{port_string}{path_string}{end_path}{query_string}{frag_string}"
      | ~{address query} ->
           query_string = query_to_string(query)
           "mailto:{address}{query_string}"

}}
