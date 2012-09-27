/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

package stdlib.apis.github.user
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub user API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */


@private GHUp = {{

  @private GP = GHParse

  one_full_user(res:WebClient.success(string)) = GP.dewrap_whole_obj(res, GP.get_user)

  multiple_users(res:WebClient.success(string)) = GP.multiple_objects(res, GP.get_user)

  multiple_emails(res:WebClient.success(string)) = GP.list_strings(res)

  one_public_key(res:WebClient.success(string)) = GP.dewrap_whole_obj(res, GP.get_public_key)
  multiple_public_keys(res:WebClient.success(string)) = GP.multiple_objects(res, GP.get_public_key)

}}

GHUser = {{

  @private GP = GHParse

  /* Users */

  get_user(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"")
    GHLib.api_get(path, data, GHUp.one_full_user)

  update_user(token:string,
              name:option(string), email:option(string), blog:option(string),
              company:option(string), location:option(string), hireable:option(string), bio:option(string)) =
    json = GHLib.mkopts([{sopt=("name",name)},{sopt=("email",email)},{sopt=("blog",blog)},{sopt=("company",company)},
                         {sopt=("location",location)},{sopt=("hireable",hireable)},{sopt=("bio",bio)}])
    GHLib.api_patch_string("/user", token, json, GHUp.one_full_user)

  /* Emails */

  get_emails(token:string) =
    GHLib.api_get_full("/user/emails", token, [], GHUp.multiple_emails)

  add_emails(token:string, emails:list(string)) =
    json = GHLib.mkslst(emails)
    GHLib.api_post_string("/user/emails", token, json, GHUp.multiple_emails)

  remove_emails(token:string, emails:list(string)) =
    json = GHLib.mkslst(emails)
    GHLib.api_delete_string("/user/emails", token, json, GP.expect_204)

  /* Follow(/ing/ers) */

  get_following(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"following")
    GHLib.api_get(path, data, GP.multiple_short_users)

  get_followers(user:GitHub.user_id) =
    (path, data) = GHLib.userpath(user,"followers")
    GHLib.api_get(path, data, GP.multiple_short_users)

  is_following(token:string, other_user:string) =
    GHLib.api_get_full("/user/following/{other_user}", token, [], GP.expect_204_404)

  follow(token:string, user:string) =
    GHLib.api_put_string("/user/following/{user}", token, "", GP.expect_204)

  unfollow(token:string, user:string) =
    GHLib.api_delete_string("/user/following/{user}", token, "", GP.expect_204)

  /* Public keys */

  get_keys(token:string) =
    GHLib.api_get_full("/user/keys", token, [], GHUp.multiple_public_keys)

  get_key(token:string, id:int) =
    GHLib.api_get_full("/user/keys/{id}", token, [], GHUp.one_public_key)

  add_key(token:string, title:string, key:string) =
    json = GHLib.mkopts([{sreq=("title",title)},{sreq=("key",key)}])
    GHLib.api_post_string("/user/keys", token, json, GHUp.one_public_key)

  update_key(token:string, id:int, title:string, key:string) =
    json = GHLib.mkopts([{sreq=("title",title)},{sreq=("key",key)}])
    GHLib.api_patch_string("/user/keys/{id}", token, json, GHUp.one_public_key)

  remove_key(token:string, id:int) =
    GHLib.api_delete_string("/user/keys/{id}", token, "", GP.expect_204)

}}
