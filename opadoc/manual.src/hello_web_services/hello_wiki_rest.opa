import stdlib.tools.markdown

database stringmap(string) /wiki
database /wiki[_] = "This page is empty. Double click to edit"

exposed function load_source(topic) {
    /wiki[topic];
}

exposed function load_rendered(topic) {
    source = load_source(topic);
    Markdown.xhtml_of_string(Markdown.default_options, source);
}

exposed function save_source(topic, source) {
    /wiki[topic] <- source;
    load_rendered(topic);
}

exposed function remove_topic(topic) {
    Db.remove(@/wiki[topic]);
}

function edit(topic) {
    Dom.set_value(#edit_content, load_source(topic));
    Dom.hide(#show_content);
    Dom.show(#edit_content);
    Dom.give_focus(#edit_content);
}

function save(topic) {
    content = save_source(topic, Dom.get_value(#edit_content));
    #show_content = content;
    Dom.hide(#edit_content);
    Dom.show(#show_content);
}

function display(topic) {
   Resource.styled_page("About {topic}", ["/resources/css.css"],
     <div id=#header><div id=#logo></div>About {topic}</div>
     <div class="show_content" id=#show_content ondblclick={function(_) { edit(topic) }}>
       {load_rendered(topic)}
     </>
     <textarea class="edit_content" id=#edit_content style="display:none"
       cols="40" rows="30" onblur={function(_) { save(topic) }}></>
    );
}

function rest(topic) {
    match (HttpRequest.get_method()) {
    case {some: method} :
        match (method) {
        case {post}:
            _ = save_source(topic, HttpRequest.get_body() ? "");
            Resource.raw_status({success});
        case {delete}:
            remove_topic(topic);
            Resource.raw_status({success});
        case {get}:
            Resource.raw_response(load_source(topic), "text/plain", {success});
        default:
            Resource.raw_status({method_not_allowed});
        }
    default:
        Resource.raw_status({bad_request});
    }
}

function topic_of_path(path) {
    String.capitalize(
        String.to_lower(List.to_string_using("", "", "::", path))
    );
}

function start(url) {
    match (url) {
    case {path: [] ... }: display("Hello");
    case {path: ["_rest_" | path] ...}: rest(topic_of_path(path));
    case {~path ...}: display(topic_of_path(path));
    }
}

Server.start(
    Server.http,
    [ {resources: @static_include_directory("resources")}
      , {dispatch: start}
    ]
)
