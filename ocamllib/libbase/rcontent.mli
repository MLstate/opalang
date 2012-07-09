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
(** Content.
    Allow HTML body data to reside either on disc or in memory.
    Note that we can't call this module "Content" because of the module of that name
    in social_network.
*)

(** Holding datatype
    We keep an optional stat for the file in case of multiple length calls.
    We don't really need ContentNone since it is exactly equivalent to (ContentString "") but
    when we finally send the response, ContentNone doesn't call the write function whereas (ContentString "") does.
*)
type content =
    ContentString of string
  | ContentBuffer of Buffer.t
  | ContentFBuffer of FBuffer.t
  | ContentFile of string * in_channel option * out_channel option * Unix.stats option * bool
  | ContentNone

(** Indicator type for buffer types, used by content_make.
*)
type content_type = CT_STRING | CT_BUFFER | CT_FBUFFER | CT_FILE | CT_NONE

(** Turn content_type into string.
*)
val string_of_content_type : content_type -> string

(** content_temporary_files:
    List of temporary files created by content_force_file etc.
*)
val content_temporary_files : string list ref

(** content_unlink_temporary_files:
    Delete all of the temporary files created by this session.
*)
val content_unlink_temporary_files : unit -> unit

(** content_make content_type hint:
    Create a content of the given type with given hint size.
    Note that the hint is ignored for files unless truncate is set to true (default is false).
    Extreme caution should be used with truncate.
*)
val content_make : ?truncate:bool -> ?hint:int -> content_type -> content

(** content_unallocate content:
    Release any resources built into content (file handles, memory etc.)
*)
val content_unallocate : content -> unit

(** content_add str content:
    Add a string to the end of content.  Imperative for Buffer, functional for FBuffer...
    Adding to a to ContentNone returns a ContentString, adding to a ContentString returns a
    ContentBuffer, adding to a buffer of more than max_buffer_size returns a ContentFile.
    The max_buffer_size parameter default is 10Mb.  ContentFBuffers grow indefinitely.
*)
val content_add : ?evolve:bool -> ?max_buffer_size:int -> string -> content -> content

(** content_add_content c1 c2:
    Add the contents of c2 to c1.
*)
val content_add_content : ?max_buffer_size:int -> content -> content -> content

(** content_length content:
    Return the size of the content, uses the stat, if present.
*)
val content_length : content -> int

(** get_contencontent_type content:
    Return the content_type.
*)
val get_content_type : content -> content_type

(** get_content content:
    Return the content as a string, reads file, if necessary.
*)
val get_content : content -> string

(** bodystr ?max_body ?escaped ?hex content:
    Return a length limited string for content.
    Length limited to max_body (default: 50).
    Only reads max_body from file.
    Terminates string with "..." if content is truncated.
    Optionally converts to hex bytes.
*)
val bodystr : ?max_body:int -> ?escaped:bool -> ?hex:bool -> content -> string

(** content_is_string content: Predicate for string content (includes (f)buffer and none). *)
val content_is_string : content -> bool

(** content_is_buffer content: Specific predicate for buffer content. *)
val content_is_buffer : content -> bool

(** content_is_fbuffer content: Specific predicate for fbuffer content. *)
val content_is_fbuffer : content -> bool

(** content_is_file content: Predicate for file content. *)
val content_is_file : content -> bool

(** content_force_string content:
    Ensure content is string-based, reads file, if necessary.
*)
val content_force_string : ?unallocate:bool -> content -> content

(** content_from_file filename:
    Create content from file.
*)
val content_from_file : ?unlinkable:bool -> ?stat:bool -> string -> content

(** content_force_file content:
    Ensure content is file-based.
    Creates temporary file according to Filename.temp_file.
    Writes string to file.
    Temp file name can be read back from datatype.
*)
val content_force_file : ?close:bool -> content -> content

(** content_rename_file:
    Creates the given target file from content.
    For ContentFile contents:
    - Rename internal file to target name.
    - The force arg and return int as per File.mv.
    For other content types, a file is created and the content dumped to the file.
    Note that if you call this function, Rcontent assumes no further reponsability
    for any internal designated file.
*)
val content_rename_file : ?force:bool -> content -> string -> int

(** content_md5 content:
    Return MD5 string for content.
    Does not read file in.
*)
val content_md5 : content -> string
