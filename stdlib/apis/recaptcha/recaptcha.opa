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
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * ReCaptha API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

import stdlib.apis.common

/**
 * {1 About this module}
 *
 * This module provides a simple way to add a ReCaptcha module in your page.
 * Important :
 *   Any application using this must respect ReCatcha terms of service,
 *   available here: http://www.google.com/recaptcha/terms
 *   
 *
 * {1 Where should I start?}
 *
 * You should first get a public and a private key from ReCatpcha:
 * http://www.google.com/recaptcha/whyrecaptcha
 * Once you have these, you can display a simple complete ReCaptcha
 * element with {!Recaptcha.show}. If you wish to customize the way
 * it looks, you can display the picture only with {!ReCaptcha.pic}
 * and the input with {!ReCaptcha.input}.
 * If you want another picture, just call {!ReCaptcha.refresh}.
 * To check if the text entered by the user is correct, you should use
 * {!ReCaptcha.check}. This function requires the IP address of the
 * user. This can be obtained with {!Server.ip_string_of_connexion}.
 * Note that the uid of {!ReCaptcha.config} represents the displayed ReCaptcha.
 * You must use the same for {!ReCaptcha.show} and {!ReCaptcha.check}
 * if you want it to work. This id does not have to be somewhere in the DOM
 * of your page and can be shared across multiple clients, but since other ids
 * will be generated from it, don't use the same for multiple captchas on the
 * same page.
 */

/**
 * A configuration of the forecast request.
 * - [public_key] is obtained from ReCaptcha
 * - [private_key] is obtained from ReCaptcha
 * - [uid] is the id of the ReCaptcha element built
**/
type ReCaptcha.config = {
  public_key  : string
  private_key : string
  uid         : string
}

ReCaptcha = {{

  @private img_id(conf:ReCaptcha.config) = "{conf.uid}__img"
  @private challenge_id(conf:ReCaptcha.config) = "{conf.uid}__challenge"
  @private response_id(conf:ReCaptcha.config) = "{conf.uid}__response"

  @private recaptcha_parser =
    spc = parser [ \t]* -> void
    apos = parser ['] -> void
    in_str = parser !apos . -> void
    parser
    | spc n=([a-z_]*) spc ":" spc "'" v=(in_str*) "'" (.*)
       -> (Text.to_string(n), Text.to_string(v))

  @private get_challenge(conf:ReCaptcha.config) =
    uri = Option.get(Uri.of_string("http://www.google.com/recaptcha/api/challenge?k={conf.public_key}"))
    match WebClient.Get.try_get(uri) with
    | {failure=_} -> do jlog("ReCaptcha request failed"); ""
    | ~{success} ->
      aux(line:string, res:string) =
        (n,v) = Parser.try_parse(recaptcha_parser, line) ? ("","")
        if n == "challenge" then v else res
      ch = List.fold(aux, String.explode("\n", success.content), "")
      do if ch == "" then jlog("ReCaptcha request failed");
      ch

  @publish @private get_challenge_and_pic(conf:ReCaptcha.config) =
    ch = get_challenge(conf)
    (ch, <img id={img_id(conf)} alt="Challenge ReCaptcha" src="http://www.google.com/recaptcha/api/image?c={ch}" />)

  /**
   * Returns a ReCaptcha picture. A hidden span is also returned.
  **/
  pic(conf:ReCaptcha.config) =
    (ch, pic) = get_challenge_and_pic(conf)
    <>
      {pic}
      <span style="display: none;">
        <input type="hidden" value={ch} id={challenge_id(conf)} name={challenge_id(conf)} />
      </span>
    </>

  /**
   * Returns the input where your user will put his ReCaptcha solution
   * It's just an input with the right id based on uid.
  **/
  input(conf:ReCaptcha.config) =
    <input type="text" id={response_id(conf)} name={response_id(conf)} autocomplete="off" />

  /**
   * Generates a new ReCaptcha picture and replaces existing one
   *
   * @param uid Unique id representing this ReCaptcha
   * @param public_key Your ReCaptcha public key
  **/
  refresh(conf:ReCaptcha.config) =
    (ch, pic) = get_challenge_and_pic(conf)
    _ = Dom.put_replace(#{img_id(conf)}, Dom.of_xhtml(pic))
    do Dom.set_value(#{challenge_id(conf)},ch)
    Dom.set_value(#{response_id(conf)},"")

  /**
   * Returns a full ReCaptcha element. This is the simplest way to display a
   * ReCaptcha, but it's just a layout suggestion. If you do not like the way
   * it looks, feel free to build your own layout with {!ReCaptcha.pic} and
   * {!ReCaptcha.input}.
   *
   * @param uid Unique id representing built ReCaptcha
   * @param public_key Your ReCaptcha public key
  **/
  show(conf:ReCaptcha.config) =
    <>
    <table style="border-width: 1px;border-style: solid;background: #810600;"><tbody>
      <tr><td colspan="3"><div style="margin:auto; width:300px">
        {pic(conf)}
      </div></td></tr>
      <tr>
        <td rowspan="2" style="background: #FFDC73;"><div>
          <label for={response_id(conf)}><span>Type the two words:</span></label>
          <br/>
          {input(conf)}
        </div></td>
        <td><a onclick={_ -> refresh(conf)} title="Get a new challenge">
          <img width="25" height="16" style="border: 0 none;" src="http://www.google.com/recaptcha/api/img/red/refresh.gif" alt="Get a new challenge" />
        </a></td>
        <td rowspan="2" style="background:url('http://www.google.com/recaptcha/api/img/red/sprite.png') no-repeat scroll -214px 0 transparent;height: 50px;width: 95px;"></td>
      </tr> 
      <tr>
        <td><a href="http://www.google.com/recaptcha/help" target="_blank" title="Help">
          <img width="25" height="16" style="border: 0 none;" src="http://www.google.com/recaptcha/api/img/red/help.gif" alt="Help" />
        </a></td>
      </tr>
    </tbody></table>
    </>

  @publish @private innerCheck(conf:ReCaptcha.config, ip:string) =
    challenge = Dom.get_value(#{challenge_id(conf)})
    response = Dom.get_value(#{response_id(conf)})
    if (challenge == "" || response == "") then false
    else
      data = [ ("privatekey", conf.private_key), ("remoteip", ip)
             , ("challenge", challenge), ("response", response)
             ] |> API_libs.form_urlencode
      uri = Option.get(Uri.of_string("http://www.google.com/recaptcha/api/verify"))
      match WebClient.Post.try_post(uri, data) with
      | {failure=_} -> do jlog("ReCaptcha request failed"); false
      | {success=s} -> List.head(String.explode("\n", s.content)) == "true"

  /**
   * Checks if the solution provided by the user is correct.
   * Returns true if correct.
   *
   * @param ip IP address of the client
  **/
  check(conf:ReCaptcha.config, ip:string) =
    innerCheck(conf, ip)

}}

