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
 * Represents an uploaded file.
 */
type Upload.file = {
  /** Name of input corresponding to the uploaded file. */
  name : string;

  /** Name of the uploaded file. */
  filename : string;

  /** Content of the file. */
  content : -> {partial:int}/{content:binary};

  /** A function that allows to fold on headers. */
  fold_headers : (forall('a). 'a, (string, string, 'a -> 'a) -> 'a)
}

/**
 * Represents other (no file) fields of form.
 */
type Upload.field = {
  /** Name of field */
  name : string;

  /** Value of field */
  value : string;
}

/**
 * An uploaded data can be a file or a field.
 */
type Upload.data = Upload.file / Upload.field

/**
 * Configuration for create an uploader.
 */
type Upload.config('result) = {
  /** Parameters for the dynamic url was created by the upload
      manager. */
  url_parameters : DynamicResource.parameters

  /** The xhtml inserted on upload form. Beware if you set this config
      fields take care to add a submit button. */
  body_form : xhtml

  /** Initial result value. */
  init_result : 'result

  /** A function that fold on datas by incomming order. */
  fold_datas : Upload.data, 'result -> 'result

  /** Perform result of datas folding. */
  perform_result : 'result -> void
}

/**
 * This module provides one main [make] function which create an xhtml
 * that allows to upload file(s) from client to server.
 */
Upload = {{
  /**
   * The default configuration :
   * - url_parameters : [{expiration={none} consumption={unlimited}
         visibility={current_context}}]
   * - fold_datas : return previous result, do nothing with data.
   * - perform_result : do nothing with data
   * - body_form : An xhtml that contains [<input type="file"
       name="filename"/><input type="submit" value="Upload"/>]. It
       be able to upload one file.
   */
  default_config(init_result:'result):Upload.config('result) = {
    url_parameters = {expiration={none} consumption={unlimited} visibility={current_context}}
    body_form = <input type="file" name="filename"/><input type="submit" value="Upload"/>
    ~init_result
    fold_datas(_data, result) = result
    perform_result(_result) = void
  }

  /**
   * Create an upload manager.
   * TODO : Add more documentation...(dynamic resource etc...)
   */
  make(config:Upload.config) =
    /* Save creation page context because iframe is another page. */
    reset_context =
      match ThreadContext.get({current})
      | {key=~{client} request=_ details=_} ->
        key = ~{client}
        ( -> { ThreadContext.get({current}) with ~key })
      | _ -> ( -> ThreadContext.get({current}))
    /* The dynamic resource use for reply to upload. */
    dynamic(request) =
      @with_thread_context(reset_context(),
        match HttpRequest.Generic.get_multipart(request) with
        | {none} ->
          Resource.error_page("Upload fail",
            <h1>Unexpected load request</h1>, {forbidden})
        | {some = multipart} ->
          result = HttpRequest.Generic.fold_multipart(multipart, config.init_result,
               (part, fh, acc ->
                 match part
                 | ~{filename name content} ->
                   /* An hack for forall, TODO make it proprely */
                   fold_headers = @unsafe_cast(fh)
                   file = ~{filename name content fold_headers}
                   config.fold_datas(file, acc)
                 | {name=_ value=_} as x->
                   config.fold_datas(x, acc)
               )
             )
          do Scheduler.push(-> config.perform_result(result))
          Resource.source("Upload success", "text/plain")
        )
    resource = Resource.dynamic(dynamic)
    upload_url = DynamicResource.publish(resource, config.url_parameters)
    idframe = Random.string(10)
    <iframe name={idframe}
            id={idframe} src="{fake_url}"
            style="width:0;height:0;border:0px solid #fff;"/>
    <form action="{upload_url}" id="upload_form" target={idframe}
          method="post" enctype="multipart/form-data">
    {config.body_form}
    </form>;

  @private dummy_page = Resource.raw_text("")
  @private @publish
  fake_url = DynamicResource.publish(dummy_page, {consumption={unlimited}; expiration={none}; visibility={shared}})
}}
