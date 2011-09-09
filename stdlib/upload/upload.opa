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

package stdlib.upload

/**
 * @author Quentin Bourgerie
 * @author Adam Koprowski (re-packaging, docs, re-implementation)
 *
 *
 * {1 About this module}
 *
 * This module provides functionality to create a form containing
 * a file upload field.
**/

/**
 * Representation of an uploaded file.
 */
type Upload.file = {

  /** Name of the input field corresponding to the uploaded file. */
  field_name : string

  /** Name of the uploaded file. */
  filename : string

  /** The mimetype of the file */
  mimetype : string

  /** Content of the file. */
  content : binary
}

/**
 * A data of the form created with this module
 */
type Upload.form_data = {
   /** data of the uploaded file (if missing then something went wrong
       with form submission) */
  uploaded_files : stringmap(Upload.file)

   /** data of other fields (map from field id to field value) */
  form_fields : stringmap(string)
}

/**
 * Configuration of the file uploader.
 */
type Upload.config = {

  /** id of the form */
  form_id : string

  /** Parameters for the dynamic url to be created by the upload manager. */
  url_parameters : DynamicResource.parameters

  /** The content of the form.
    * It can contain many fields, among which there should be one field
    * for file upload:
    * {[
    * <input type="file" id="..." name="..." />
    * ]}
    */
  form_body : xhtml

  /** A function that will be invoked upon uploading the file */
  process : Upload.form_data -> void
}

/**
 * This module provides one main [make] function which create an xhtml
 * that allows to upload file(s) from client to server.
 */
Upload = {{

  /**
   * The default configuration of a file upload widget.
   * It creates a form with only one file upload field and
   * does nothing upon retrieving the file.
   */
  default_config() : Upload.config = {
    form_id = Dom.fresh_id()
    url_parameters = {
      expiration={none}
      consumption={unlimited}
      visibility={current_context}
    }
    form_body =
      <input type="file" name="filename"/>
      <input type="submit" value="Upload"/>
    process(_) = void
  }

  /* Folding on the data of the multipart form data */
  @private
  multifold(part, fold_headers, form_data) =
    match part with
     // fileupload field -- process
    | ~{filename name content} ->
         find_mime(header, value, acc) =
           match header with
           | "Content_Type" -> value
           | _ -> acc
         /* A hack for forall, TODO make it proprely */
         mimetype = @unsafe_cast(fold_headers)("text/plain", find_mime)
         field_name = name
         full_content =
           rec aux() =
             match content() with
             | {partial=_} ->
                 do Scheduler.wait(500)
                 aux()
             | ~{content} -> content
            aux()
          file = ~{filename field_name mimetype content=full_content}
          { form_data with
              uploaded_files=Map.add(name, file, form_data.uploaded_files) }
     // "regular" field -- ignore
    | ~{name value} ->
         { form_data with
             form_fields=Map.add(name, value, form_data.form_fields) }

  /**
   * Creates a form with file upload capabilities.
   *
   * @param config Configuration of the file-upload widget.
   */
  html(config : Upload.config) : xhtml =
    /* Save creation page context because iframe is another page. */
    reset_context =
      match ThreadContext.get({current}) with
      | {key=~{client} request=_ details=_ constraint=_} ->
          key = ~{client}
          ( -> { ThreadContext.get({current}) with ~key })
      | _ ->
          ( -> ThreadContext.get({current}))
    /* The dynamic resource used for the upload. */
    dynamic(request) =
      @with_thread_context(reset_context(),
        match HttpRequest.Generic.get_multipart(request) with
        | {none} ->
            Resource.error_page("Upload failed", <h1>Unexpected load request</h1>,
              {forbidden})
        | {some = multipart} ->
            empty_form_data = { form_fields=Map.empty uploaded_files=Map.empty }
            form_data =  HttpRequest.Generic.fold_multipart(multipart,
                           empty_form_data, multifold)
            do Scheduler.push( -> config.process(form_data))
            Resource.source("Upload successful", "text/plain")
        )
    resource = Resource.dynamic(dynamic)
    upload_url = DynamicResource.publish(resource, config.url_parameters)
    frame_style = css {
      width: 0px;
      height: 0px;
      display: none;
      border: 0px solid white
    }
    frameId = Dom.fresh_id()
    <>
      <iframe name={frameId} id={frameId} src="#" style={frame_style} />
      <form id={config.form_id} action="{upload_url}" target={frameId}
            method="post" enctype="multipart/form-data">
        {config.form_body}
      </form>
    </>

}}
