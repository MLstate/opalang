/*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.{crypto, web.mail}

/**
 * Type of a default image to be served by the Gravatar service.
 * Below descriptions taken from: http://en.gravatar.com/site/implement/images/
 */
type Gravatar.default_image =
  { ret_404 } /       /** do not load any image if none is associated with the email
                          hash, instead return an HTTP 404 (File Not Found) response */
  { mystery_man } /   /** a simple, cartoon-style silhouetted outline of a person
                          (does not vary by email hash) */
  { identicon } /     /** a geometric pattern based on an email hash */
  { monsterid } /     /** a generated 'monster' with different colors, faces, etc */
  { wavatar } /       /** generated faces with differing features and backgrounds */
  { retro } /         /** awesome generated, 8-bit arcade-style pixelated faces */
  { custom: Uri.uri } /** Custom, user-supplied default */

/**
 * Filtering profile images by self-imposed ratings.
 * Below descriptions taken from: http://en.gravatar.com/site/implement/images/
 */
type Gravatar.rating =
  { G } /   /** suitable for display on all websites with any audience type. */
  { PG } /  /** may contain rude gestures, provocatively dressed individuals,
                the lesser swear words, or mild violence. */
  { R } /   /** may contain such things as harsh profanity, intense violence,
                nudity, or hard drug use. */
  { X }     /** may contain hardcore sexual imagery or extremely disturbing violence. */

/**
 * Various options for an image request to the Gravatar service
 */
type Gravatar.option =
  { size : int } /                            /** Image size (image is square) */
  { default_img : Gravatar.default_image } /  /** Default image type */
  { forceDefault } /                          /** Force default image */
  { rating : Gravatar.rating } /              /** Filtering by image ratings */
  { secure }                                  /** Forces serving via SSL */

/**
 * This module provides API for the Gravatar service, providing profile
 * images that are easy to use across many different sites.
 */
Gravatar = {{

  @stringifier(Gravatar.default_image) default_to_string(default) =
    match default with
    | {ret_404} -> "404"
    | {mystery_man} -> "mm"
    | {identicon} -> "identicon"
    | {monsterid} -> "monsterid"
    | {wavatar} -> "wavatar"
    | {retro} -> "retro"
    | {custom=uri} -> Uri.to_string(uri) |> Uri.encode_string

  @stringifier(Gravatar.rating) rating_to_string(rating) =
    match rating with
    | {G} -> "g"
    | {PG} -> "pg"
    | {R} -> "r"
    | {X} -> "x"

  @private mk_option =
  | ~{size} -> ("s", String.of_int(size))
  | ~{default_img} -> ("d", "{default_img}")
  | {forceDefault} -> ("f", "y")
  | ~{rating} -> ("r", "{rating}")
  | {secure} -> error("Should not happen; cannot make option of \{secure}")

   /**
    * Returns an URL to a Gravatar image.
    *
    * @param email User's email address
    * @param options List of options for the generated image
    * @return An URL to the profile image associated with [email] and with
    *         the requested [options].
    */
  mk_url(email : Email.email, options : list(Gravatar.option)) : Uri.uri =
    secure = List.mem({secure}, options)
    email_hash =
      Email.to_string_only_address(email)
      |> String.trim |> String.to_lower |> Crypto.Hash.md5
    uri =
      { Uri.default_absolute with
        schema = some(if secure then "https" else "http")
        domain = if secure then "secure.gravatar.com" else "www.gravatar.com"
        path = ["avatar", email_hash]
        query = List.map(mk_option, List.remove({secure}, options))
      }
    Uri.of_absolute(uri)

}}
