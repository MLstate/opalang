(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
(** userCompatType:
    Type of info extracted from the user agent string in http request headers.
*)

type environment_interface_engine =
  | X11         (** The browser runs under Linux, FreeBSD, OpenBSD, etc. *)
  | Windows     (** The browser runs under Windows. *)
  | Macintosh   (** The browser runs on a Macintosh (either MacOS X or Classic but not iPhone OS). *)
  | IPhone      (** The browser runs under iPhone OS, i.e. an iPhone or an iPod touch. *)
  | Symbian     (** The browser runs under a Symbian OS *)
  | J2ME        (** Java-based mobile phone, eg Opera-mini *)
  | WII         (** Nintendo's weird OS *)
  | PLAYSTATION (** PSP etc. *)
  | BeOS        (** Does this still exist? *)
  | UnidentifiedEIE

let string_of_eie = function
  | X11 -> "X11"
  | Windows -> "Windows"
  | Macintosh -> "Macintosh"
  | IPhone -> "IPhone"
  | Symbian -> "Symbian"
  | J2ME -> "J2ME"
  | WII -> "WII"
  | PLAYSTATION -> "PLAYSTATION"
  | BeOS -> "BeOS"
  | UnidentifiedEIE -> "UnidentifiedEIE"

type webkit_variant =
  | Safari of int list 
  | Chrome of int list 
  | UnidentifiedWV

let sil l = String.concat "." (List.map string_of_int l)

let string_of_webkit_variant = function
  | Safari l -> "Safari/"^(sil l)
  | Chrome l -> "Chrome/"^(sil l)
  | UnidentifiedWV -> "UnidentifiedWV"

type bot_engine =
  | Googlebot of int list (** Google's indexing engine.    *)
  | Msnbot of int list    (** Microsoft's indexing engine. *)
  | Yahoobot              (** Yahoo!'s indexing engine.    *)

let string_of_bot_engine = function
  | Googlebot l -> "Googlebot/"^(sil l)
  | Msnbot l -> "Msnbot/"^(sil l)
  | Yahoobot -> "Yahoobot"

type renderer_engine =
  | Bot of bot_engine     (** This is not a browser but a bot. *)
  | Gecko of int list     (** Gecko-based browser, e.g. Firefox.  *)
  | KHTML of int list     (** Gecko-like browser, e.g. Konqueror.  *)
  | Trident of int list   (** Trident-based browser, e.g. Internet Explorer.*)
  | Webkit of int list * webkit_variant (**WebKit-based browser, e.g. Safari or Google Chrome.*)
  | Nokia of int list     (** Mobile phones? *)
  | Presto of int list    (** Presto-based browser, e.g. Opera. *)
  | Text of string * int list (** W3M,links,lynx etc. not OPA compatible. Generic class for non-graphics-based browsers? *)
  | Dillo of int list     (** Weird browser, unique engine. Probably not compatible with OPA but it's on a lot of mini-systems. *)
  | PS of int list        (** Playstation and friends *)
  | Wii of int list       (** Nintendo *)
  | UnidentifiedRE

let string_of_renderer_engine = function
  | Bot be -> string_of_bot_engine be
  | Gecko l -> "Gecko/"^(sil l)
  | KHTML l -> "KHTML/"^(sil l)
  | Trident l -> "Trident/"^(sil l)
  | Webkit (l,wv) -> "Webkit/"^(sil l)^" "^(string_of_webkit_variant wv)
  | Nokia l -> "Nokia/"^(sil l)
  | Presto l -> "Presto/"^(sil l)
  | Text (name,l) -> name^"/"^(sil l)
  | Dillo l -> "Dillo/"^(sil l)
  | PS l -> "PS/"^(sil l)
  | Wii l -> "Wii/"^(sil l)
  | UnidentifiedRE -> "UnidentifiedRE"

type resolution = { width: int; height: int }

let string_of_resolution { width=width; height=height } =
  "{width="^(string_of_int width)^"; height="^(string_of_int height)^"}"

type user_compat = { environment: environment_interface_engine;
                     renderer: renderer_engine }

let string_of_user_compat { environment=environment; renderer=renderer } =
  "{environment="^(string_of_eie environment)^"; renderer="^(string_of_renderer_engine renderer)^"}"
