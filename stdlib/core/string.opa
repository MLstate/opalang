/*
    Copyright © 2011 MLstate

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
 * @author Nicolas Pelletier, 2010
 * @author Valentin Gatien-Baron, 2010 (documentation)
 * @destination public
 * @stability unknown
 * @category data
 */

/**
 * {1 About this module}
 *
 * Definition of the string library
 *
 * Strings are immutable containers of characters.
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/*
 * TODO:
 * define string_insert in opa
 * define string_escaped in ??
 */

/**
 * {1 Types defined in this module}
 */

/**
 * Order type label for string
 */
type String.order = Order.default
@abstract type String.order_ci = void

/**
 * {1 Interface}
 */

String =
{{
  /**
   * {2 Primitive operations}
   */

  /**
   * Compare two string
   */
  compare(a,b): Order.comparison =
    @opensums(ordering(a,b))


  /**
   * Compare two string
   */
  ordering = %%BslString.ordering%%

  compare_raw = %% BslPervasives.compare_string %%

  /**
   * Check equality of two string
   */
  equals = eq
  /*
   * cannot call these functions <, > etc. because
   * then the rest of the module cannot call the polymorphic >
   */
  `lt` = %%BslString.lt%%
  `gt` = %%BslString.gt%%
  `le` = %%BslString.leq%%
  `ge` = %%BslString.geq%%
  `ne` = %%BslString.neq%%
  `eq` = %%BslString.eq%%
  `^`  = %% BslString.concat %%

  /**
   * Returns the length of the string
   */
  length = %% BslString.length %%

  /**
   * Determine if a string is empty
   *
   * @return true if the string is empty, false otherwise
   */
  is_empty(s): bool =
     length(s) == 0

  /**
   * Returns the string composed of the nth character of the given string
   * (starting at index 0)
   *
   * Exits with an error when the index is out of bounds
   */
  get(offset: int, source: string): string=
    f= %% BslString.get %%
    if offset < length(source) && offset >= 0 then f(source, offset)
    else error("get out of range")
  : string

  /**
   * [index(substring, string)] finds a sub-string in a string
   *
   * @return {none} if [substring] doesn't appear in [string]
   * @return {some} containing the index of the first occurrence of the
   * sub-string otherwise
   */
  index = %% BslString.index %%

  /**
   * [init(f, n)] creates a string of length [n] containing the characters [f(0)],
   * [f(1)], ..., [f(n-1)]
   */
  init = %% BslString.init %%

  /**
   * [make(n, c)] returns a string of length [n], filled with the character [c].
   */
  make(n, c) =
    init((_index -> c), n)

  /**
   * [replace(substring, replacement, source)] replaces every occurrence of
   * [substring] by [replacement]
   *
   * Characters from the input cannot belong to several overlapping occurrences
   * of the sub-string
   * eg: [replace("aba","c","ababa")] will return ["cba"]
   */
  replace = %% BslString.replace %%

  /**
   * Create a string in the reverse order of the original one
   */
  reverse = %% BslString.reverse %%

  /**
   * [substring_opt(offset, length, source)] returns an option
   * containing the string composed of the characters of [source],
   * starting at [offset] (indexed from 0) and of length [length]
   *
   * Returns [Option.none] when [length(source) < offset+length]
   */
  substring_opt(offset: int, len: int, source: string): option(string) =
  (
    src_len= length(source)
    if len < 0 || offset < 0 || offset + len > src_len then
      Option.none
    else
      // if we suppose that the previous verification are enough then this call must always work
      Option.some(substring_unsafe(offset, len, source))
  )

  /**
   * A variant of [substring] that can raise an exception if offsets are incorrect
   */
  substring_unsafe(offset:int, len:int, source:string): string =
     llsub = %% BslString.sub %%
     src_len = length(source)
     if offset == 0 && src_len == len then source
     else llsub(offset, len, source)

  /**
   * [substring(offset, length, source)] returns the string composed of the
   * characters of [source], starting at [offset] (indexed from 0) and of length
   * [length]
   *
   * Exits with an error when [length(source) < offset+length]
   */
  substring(offset: int, len: int, source: string)=
    match (substring_opt(offset, len, source)) : option(string) with
      | { ~some } -> some
      | { none } -> error("[substring] out of range in \"{source}\" ({offset}, {len}, {length(source)} )")
  : string

  /**
   * Gets a prefix of a given string of given length.
   *
   * @param length prefix length
   * @param source source string
   * @return [length]-character length prefix of [source] or [none] if
   *         [source] is shorter than [length] characters.
   */
  get_prefix(length: int, source: string) : option(string) =
    String.substring_opt(0, length, source)

  /**
   * Gets a suffix of a given string of given length.
   *
   * @param length suffix length
   * @param source source string
   * @return [length]-character length suffix of [source] or [none] if
   *         [source] is shorter than [length] characters.
   */
  get_suffix(length: int, source: string) : option(string) =
    String.substring_opt(String.length(source) - length, length, source)

  /**
   * [replace the first character by his uppercase]
   *
   * eg: [capitalize("toTo")] will return ["ToTo"]
   */
  capitalize(str: string)=
    len= length(str)
    if len < 1 then ""
    else to_upper(get(0, str))^substring(1, len - 1, str)

  /**
   * Order for string comparison
   *
   * The comparison differenciates lowercase from uppercase,
   * use the "ASCII order" to compare them.
   */
  order =
    Order.make(ordering) : order(string, String.order)

  /**
   * Order for string comparison
   *
   * Same behavior than [String.order], but case insensitive.
   */
  order_ci =
    Order.make(x,y -> ordering(to_lower(x),to_lower(y)))
    : order(string, String.order_ci)

  /**
   * Create the string representation of an integer
   */
  of_int= %% BslString.of_int %%

  /**
   * Create the string representation of a float
   */
  of_float= %% BslNumber.Float.to_string %%

  /**
   * Create the string representation of a void
   */
  of_void(_: void)= "\{\}"

  /**
   * Converts all the characters of the string to lowercase
   */
  to_lower= %% BslCactutf.lowercase %%

  /**
   * Converts all the characters of the string to uppercase
   */
  to_upper= %% BslCactutf.uppercase %%

  /**
   * Removes some accents
   */
   //FIXME 1: this should be done in a more complete way and moved to Catcutf
   //FIXME 2: the server version probably breaks UTF-8 strings
   //WARNING: used in external projects
  remove_accents = %% Bslstring.remove_accents %%

  /**
   * Escapes every characters of the string with &#xxx escapes
   *
   * Manual use of this function is strongly discouraged, as it may cause
   * security risks
   */
  escape_html= %% BslString.escapeHTML %%

  /**
   * Returns the UTF8 representation of a positive integer
   *
   * Exits with an error when the integer is negative
   */
  of_utf8_val= %% BslString.to_character %%

  /**
   * @temporary
   */
  of_ascii_val= of_utf8_val

  /**
   * Build a string from a byte value.
   * Useful if you use strings for outputs in binary format
  **/
  of_byte_val = %% BslString.of_byte_val %%

  /**
    * Escapes some characters. Used internally.
    * Very slow: replace or improve
   **/
  escape_non_utf8_special(v) =
    rpl_list = [
             /* IN FIRST IS BACKSLASH */
             ( "\\" , "\\\\"),
             ( "\n" , "\\n" ),
             ( "\r" , "\\r" ),
             ( "\t" , "\\t" ),
             ( "{Int.to_char_unsafe(0)}" , "\\000" ),
             ( "\"" , "\\\"" )
     ]
     List.foldl((pat, rpl), str -> String.replace(pat, rpl, str) , rpl_list, v)

  /**
   * {2 Additional functions}
   */

  /**
   * [replace_first(substring, replacement, source)] replaces the leftmost
   * occurrence of [substring] in [source] by [replacement] (if any)
   */
  replace_first(search: string, replacement: string, source: string)=
    lfun= offset ->
      len= offset + length(search)
      sub(0, offset, source)^replacement^drop_left(len, source)
    Option.switch(lfun, source, index(search, source))
  : string

  /**
   * the same as index but insensitive
   */
  index_i(needle: string, source: string)=
    index(to_lower(needle), to_lower(source))
    : option(int)

  /**
   * Returns the right part of [source], [len] is the left part length
   */
  drop_left(len: int, source: string)=
    sub(len, length(source) - len, source)
  : string

  /**
   * Returns the left part of [source], [len] is the right part length
   */
  drop_right(len: int, source: string)=
    sub(0, length(source) - len, source)
  : string

  /**
   * same as get but from the end
   */
  get_end(offset: int, source: string)=
    get(length(source) - offset - 1, source)
  : string

  /**
   * [check_substring(string, pos, substr)] checks that the sub-string
   * of [string] starting at [pos] begins with [substr]
   */
  check_substring : string, int, string -> bool = %% BslString.check_match_literal %%

  /**
   * Splits a string into a list of segments separated by a given string.
   *
   * @param separator a string to act as a separator
   * @param source a source string
   * @param chain_sep if true then two or more occurrences of a separator
   *        are treated as one. If false then empty strings occur between
   *        consecutive separators. I.e.
   *        [explode_with("/", "/a//b/", true) = ["a","b"]]
   *        [explode_with("/", "/a//b/", false) = ["a","","b"]]
   * @return A list of segments from the string [source] in between
   *         [separator]s.
   */
  explode_with(separator: string, source: string, chain_sep: bool) : list(string) =
    sep_len = length(separator)
    if sep_len == 0 then [ source ] else
    source_len = length(source)
    rec aux_sep(acc,pos) =
      if Int.equals(pos,source_len) then
        List.rev(acc)
      else
        if check_substring(source,pos,separator) then
          aux_sep(if chain_sep then acc else ["" | acc],pos+sep_len)
        else
          aux_not_sep(acc,pos,pos+1)
    and aux_not_sep(acc,start_pos,pos) =
      if Int.`>=`(pos,source_len) then
        acc = [substring_unsafe(start_pos,pos-start_pos,source)|acc]
        List.rev(acc)
      else
        if check_substring(source,pos,separator) then
          acc = [substring_unsafe(start_pos,pos-start_pos,source)|acc]
          aux_sep(acc,pos+sep_len)
        else
          aux_not_sep(acc,start_pos,pos+1)
    aux_sep([],0)

  /**
   * Splits a string into a list of segments separated by a given string.
   * Two consecutive occurrences of a separator are treated as one; if
   * that's not what you want check [String.explode_with].
   *
   * @param separator a string to act as a separator
   * @param source a source string
   * @return A list of segments from the string [source] in between
   *         [separator]s.
   */
  explode(separator: string, source: string) : list(string) =
    explode_with(separator, source, true)

  ws_length(s) =
    len = length(s)
    rec aux(pos) =
      if Int.equals(pos,len) then
        pos
      else
        match get(pos,s) with
        | " " | "\n" | "\r" | "\t" | /*"\v"*/"\11" | "\0" -> aux(pos+1)
        | _ -> pos
    aux(0)

  /**
   * Returns a copy of [source] string without padding which match with
   * [" ", "\n", "\r", "\t", "\v", "\0"]
   */
  strip(source: string): string=
    len   = length(source)
    left  = ws_length(source)
    if len == left then ""
    else
        right = ws_length(rev(source))
        sub(left, len - left - right, source)

  /**
   * the same as trim but strip only the left part
   */
  strip_left(source: string)=
    padding= ws_length(source)
    drop_left(padding, source)
  : string

  /**
   * the same as trim but strip only the right part
   */
  strip_right(source)=
    padding= ws_length(rev(source))
    drop_right(padding, source)
  : string

  /**
   * iter on each character of [source] and call the function [callback] with the current character
   */
  map(callback: string -> string, source: string)=
    len= length(source)
    rec aux= offset, accu ->
        if offset < len
        then aux(offset+1,accu^callback(get(offset, source)))
        else accu
    aux(0, "")
  : string

  /**
   * iter on each character of [source] and call the function [callback] with the current character
   * and an accumulator [accumulator]
   * @return the last accumulator
   */
  fold(callback:string, 'a -> 'a, source: string, accumulator: 'a)=
    len= length(source)
    rec aux= offset, accu ->
      if offset < len
        then aux(offset + 1, callback(get(offset, source), accu))
        else accu
    aux(0, accumulator)
  : 'a

  /**
   * @return true if the source contains only white spaces characters, false otherwise
   */
  is_blank(source: string)=
    if length(strip_left(source)) > 0 then false else true
  : bool

  /**
   * concatenates the list of alphas, inserting string [separator] between each.
   * each element of this list is given to callback to make a string
   */
  of_list(callback:'a -> string, separator: string, list: list('a)) : string =
    concat(separator, List.map(callback, list))

  /**
   * The same as of_list but the list must contain strings
   */
  @opacapi
  concat(separator: string, list: list(string)) : string =
    flatten(List.intersperse(separator, list))

  /**
   * The same as of_list but the list must contain strings and there is no separator
   */
  flatten(list: list(string)): string=
  (
    llcreate   = %% BslBuffer.create %%
    llappend   = %% BslBuffer.append %%
    llcontents = %% BslBuffer.contents %%
    size = List.fold(((x:string),(acc:int) -> String.length(x) + acc),  list, 0)//Compute buffer length -- may not be useful
    llbuff     = llcreate(size)
    do List.iter(s -> llappend(llbuff, s), list)
    llcontents(llbuff)
  )

  /**
   * make a string from a list of chars
   */
  of_char_list(list: list(char))=
    of_list(Char.to_string, "", list)
  : string

  /**
   * This function returns an unsigned int to count blocks of non blank
   */
  word_count(source: string)=
    lfun= char, accu -> if xor(is_blank(char), accu.f1) then (not(accu.f1), accu.f2 + 1) else accu
    (fold(lfun, source, (true, 0)).f2 + 1) / 2
  : int

  /**
   * Returns the index of unmatched char
   * Returns 0 if both strings [left] and [right] are equal
   * Returns 1 if the first char differ
   */
  diff(left: string, right: string)=
    len = Int.min(length(left), length(right))
    rec aux= offset ->
      if get(offset, left) == get(offset, right) then if offset < len - 1 then aux(offset + 1) else len
      else offset + 1
    if len > 0 then res= aux(0) if res == Int.max(length(left), length(right)) then 0 else res
    else 0
  : int

  /**
   * same as diff but case insensitive
   */
  diff_i(left: string, right: string)=
    diff(to_lower(left), to_lower(right))
  : int

  /**
   * this function completes the [source] string to get the length wanted [len] with the string [padder]
   */
  padding_left(padder: string, len: int, source: string)=
    src_len= length(source)
    pad_len= length(padder)
    if len < src_len || pad_len < 1 then error("padding left failure")
    else sub(0, len - src_len, repeat(((len - src_len) / pad_len) + 1, padder))^source
  : string

  /**
   * the same as padding by pad on right
   */
  padding_right(padder: string, len: int, source: string)=
    src_len= length(source)
    pad_len= length(padder)
    if len < src_len || pad_len < 1 then error("padding right failure")
    else source^sub(0, len - src_len, repeat(((len - src_len) / pad_len) + 1, padder))
  : string

  /**
   * Returns [source] repeated multiplier times
   */
  repeat(times: int, source: string)=
    rec aux= accu, times -> if times > 0 then aux(accu^source, times - 1) else accu
    aux(source, times - 1)
  : string

  /**
   * Returns a string that succeed [source]
   */
  next(source: string)=
    if length(source) == 0 then "a"
    else
      i = Char.to_int(Char.of_string(String.get_end(0, source)))
      if (i >= 65 && i < 90) || (i >= 97 && i < 122) then
        substring(0, length(source) - 1, source) ^
          of_char_list([default('_',Int.to_char(i+1))])
      else source ^ "a"
  : string

  /**
   * Returns true iff the source string has a given prefix.
   *
   * @param prefix a prefix to check
   * @param source a source string
   * @reutrn true iff [source] has prefix [prefix]
   */
  has_prefix(prefix: string, source: string) : bool =
    match get_prefix(length(prefix), source)
    | {none} -> false
    | {some=source_prefix} -> equals(prefix, source_prefix)

  /**
   * Returns true iff the source string has a given suffix.
   *
   * @param suffix a suffix to check
   * @param source a source string
   * @reutrn true iff [source] has suffix [suffix]
   */
  has_suffix(suffix: string, source: string) : bool =
    match get_suffix(length(suffix), source)
    | {none} -> false
    | {some=source_suffix} -> equals(suffix, source_suffix)

  /**
   *  ALIAS / SHORTHAND OF PREEXISTING FUNCTIONS
   */

  strpos             = index
  strposi            = index_i
  rev                = reverse
  sub_opt            = substring_opt
  substr             = substring
  sub                = substring
  trim               = strip
  rtrim              = strip_right
  ltrim              = strip_left
  implode            = of_list
  pad_left           = padding_left
  pad_right          = padding_right
  uppercase          = to_upper
  lowercase          = to_lower

  of_string          = identity:string -> string
  to_int             = Int.of_string
  to_float           = Float.of_string
  to_char            = Char.of_string
  to_string          = of_string

  /**
   * DEPRECATED FUNCTIONS
   */

  @deprecated({use="String.of_char_list"}) string_of_chars = of_char_list
  print_list = source -> "["^concat(",", source)^"]"
}}

/*
 * FIXME: Used ? if, @opacapi else, remove
*/
type Buffer_private.buffer = external//Low-level buffers, used internally to speed-up serialization
