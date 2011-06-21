import stdlib.web.template

uri_for_topic =
    domain_parser = {CommandLine.default_parser with
          names       = ["--wiki-server-domain"]
          description = "The REST server for this wiki. By default, localhost."
          on_param(x)     = parser y=Rule.consume -> {no_params = {x with domain = y}}
    }
    port_parser = {CommandLine.default_parser with
         names       = ["--wiki-server-port"]
         description = "The server port of the REST server for this wiki. By default, 8080."
         on_param(x)     = parser y=Rule.natural -> {no_params = {x with port = {some = y}}}
    }
    base_uri = CommandLine.filter({title     = "Wiki arguments"
     init      = {Uri.default_absolute with domain = "localhost"
                                            schema = {some = "http"}}
     parsers   = [domain_parser, port_parser]
     anonymous = []
    })
    topic -> Uri.of_absolute({base_uri with path   = ["_rest_", topic]})

@publish load_source(topic)   =
   match WebClient.Get.try_get(uri_for_topic(topic)) with
      | {failure = _} -> "Error, could not connect"
      | {~success}    -> match WebClient.Result.get_class(success) with
          | {success} -> success.content
          | _         -> "Error {success.code}"
   end

@publish load_rendered(topic) =
    source = load_source(topic)
    match Template.try_parse( Template.default, source) with
      | {failure = _} -> <>{source}</>
      | ~{success}-> Template.to_xhtml(Template.default, success)

@publish save_source(topic, source) =
   match Template.try_parse(Template.default, source) with
    | ~{success}    -> match WebClient.Post.try_post(uri_for_topic(topic), source) with
         | { failure = _ } -> {failure = "Could not reach distant server"}
         | { success = s } -> match WebClient.Result.get_class(s) with
              |  {success} -> {success = Template.to_xhtml(Template.default, success)}
              |  _         -> {failure = "Error {s.code}"}
            end
         end
    | {failure = _} -> {failure = "Incorrect syntax"}

remove_topic(topic) =
   _ = WebClient.Delete.try_delete(uri_for_topic(topic))
   void

edit(topic) =
   do Dom.transform([#show_messages <- <></>])
   do Dom.set_value(#edit_content, load_source(topic))
   do Dom.hide(#show_content)
   do Dom.show(#edit_content)
   do Dom.give_focus(#edit_content)
   void

save(topic) =
   match save_source(topic, Dom.get_value(#edit_content)) with
     | { ~success } ->
       do Dom.transform([#show_content <- success]);
       do Dom.hide(#edit_content);
       do Dom.show(#show_content);
       void
     | {~failure} ->
       do Dom.transform([#show_messages <- <>{failure}</>])
       void

display(topic) =
   Resource.styled_page("About {topic}", ["/resources/css.css"],
     <div id=#header><div id=#logo></div>About {topic}</div>
     <div class="show_content" id=#show_content ondblclick={_ -> edit(topic)}>
        {load_rendered(topic)}
     </>
     <div class="show_messages" id=#show_messages />
     <textarea class="edit_content" id=#edit_content style="display:none"
        cols="40" rows="30" onblur={_ -> save(topic)}></>
   )

rest(topic) =
(
  match HttpRequest.get_method() with
    | {some = method} ->
       match method with
         | {post}   ->
             _ = save_source(topic, HttpRequest.get_body()?"")
             Resource.raw_status({success})
         | {delete} ->
             do remove_topic(topic)
             Resource.raw_status({success})
         | {get}    -> Resource.source(load_source(topic), "text/plain")
         | _ -> Resource.raw_status({method_not_allowed})
       end
    | _ -> Resource.raw_status({bad_request})
)

topic_of_path(path) = String.capitalize(String.to_lower(
                    List.to_string_using("", "", "::", path)
))

start =
   | {path = [] ... }             -> display("Hello")
   | {path = ["rest" | path] ...} -> rest(topic_of_path(path))
   | {~path ...}                  -> display(topic_of_path(path))

server = Server.of_bundle([@static_include_directory("resources")])
server = Server.simple_dispatch(start)
