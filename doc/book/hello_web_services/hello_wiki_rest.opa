import stdlib.web.template

db /wiki: stringmap(Template.default_content)
db /wiki[_] = Template.text("This page is empty"): Template.default_content

@publish load_source(topic)   = Template.to_source(Template.default, /wiki[topic])
@publish load_rendered(topic) = Template.to_xhtml( Template.default, /wiki[topic])
@publish save_source(topic, source) =
   match Template.try_parse(Template.default, source) with
    | ~{success}    -> do /wiki[topic] <- success; Template.to_xhtml(Template.default, success)
    | {failure = _} -> do /wiki[topic] <- Template.text(source); <>Error: {source}</>
remove_topic(topic) = Db.remove(@/wiki[topic])

edit(topic) =
   do Dom.set_value(#edit_content, load_source(topic))
   do Dom.hide(#show_content)
   do Dom.show(#edit_content)
   do Dom.give_focus(#edit_content)
   void

save(topic) =
   content = save_source(topic, Dom.get_value(#edit_content))
   do Dom.transform([#show_content <- content]);
   do Dom.hide(#edit_content);
   do Dom.show(#show_content);
   void

display(topic) =
   Resource.styled_page("About {topic}", ["/resources/css.css"],
     <div id=#header><div id=#logo></div>About {topic}</div>
     <div class="show_content" id=#show_content ondblclick={_ -> edit(topic)}>
          {load_rendered(topic)}
     </>
     <textarea class="edit_content" id=#edit_content style="display:none"
          cols="40" rows="30" onblur={_ -> save(topic)}></>
   )

rest(topic) =
(
  match HttpRequest.get_method() with
    | {some = method} ->
       match method with
         | {post}   -> _ = save_source(topic, HttpRequest.get_body()?"") Resource.raw_status({success})
         | {delete} -> do remove_topic(topic) Resource.raw_status({success})
         | {get}    -> Resource.raw_response(load_source(topic), "text/plain", {success})
         | _ -> Resource.raw_status({method_not_allowed})
       end
    | _ -> Resource.raw_status({bad_request})
)

topic_of_path(path) = String.capitalize(String.to_lower(List.to_string_using("", "", "::", path)))

start =
   | {path = [] ... }               -> display("Hello")
   | {path = ["_rest_" | path] ...} -> rest(topic_of_path(path))
   | {~path ...}                    -> display(topic_of_path(path))

server = Server.of_bundle([@static_include_directory("resources")])
server = Server.simple_dispatch(start)
