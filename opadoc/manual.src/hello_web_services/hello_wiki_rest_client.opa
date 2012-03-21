import stdlib.tools.markdown

function uri_for_topic(topic) {
    Uri.of_absolute(
        {Uri.default_absolute with
            schema : {some: "http"},
         domain : "localhost",
         port   : {some: 8080},
         path   : ["_rest_", topic]
        }
    )
}

exposed function load_source(topic) {
    match (WebClient.Get.try_get(uri_for_topic(topic))) {
    case { failure : _ }: "Error, could not connect";
    case ~{ success }:
        match (WebClient.Result.get_class(success)) {
        case { success }: success.content;
        default: "Error {success.code}";
        }
    }
}

exposed function load_rendered(topic) {
    source = load_source(topic);
    Markdown.xhtml_of_string(Markdown.default_options, source);
}

exposed function save_source(topic, source) {
    match (WebClient.Post.try_post(uri_for_topic(topic), source)) {
    case { failure: _ }: {failure: "Could not reach the distant server"};
    case { success: s }:
        match (WebClient.Result.get_class(s)) {
        case { success }: {success : load_rendered(topic)};
        default: {failure : "Error {s.code}"};
        }
    }
}

function remove_topic(topic) {
    _ = WebClient.Delete.try_delete(uri_for_topic(topic));
    void;
}

function edit(topic) {
    #show_messages = <></>;
    Dom.set_value(#edit_content, load_source(topic));
    Dom.hide(#show_content);
    Dom.show(#edit_content);
    Dom.give_focus(#edit_content);
}

function save(topic) {
    match (save_source(topic, Dom.get_value(#edit_content))) {
    case { ~success }:
        #show_content = success;
        Dom.hide(#edit_content);
        Dom.show(#show_content);
    case {~failure}:
        #show_messages = <>{failure}</>;
    }
}

function display(topic) {
    Resource.styled_page(
        "About {topic}", ["/resources/css.css"],
            <div id=#header><div id=#logo></div>About {topic}</div>
            <div class="show_content" id=#show_content ondblclick={function(_) { edit(topic) }}>{load_rendered(topic)}</>
            <div class="show_messages" id=#show_messages />
            <textarea class="edit_content" id=#edit_content style="display:none" cols="40" rows="30" onblur={function(_) { save(topic) }}></>
    );
}

function rest(topic) {
    match (HttpRequest.get_method()) {
    case {some: method}:
        match (method) {
        case { post }:
            _ = save_source(topic, `?`(HttpRequest.get_body(), ""));
            Resource.raw_status({success});
        case { delete }: remove_topic(topic);
            Resource.raw_status({success});
        case { get }: Resource.source(load_source(topic), "text/plain");
        default: Resource.raw_status({method_not_allowed});
        }
    default: Resource.raw_status({bad_request});
    }
}

function topic_of_path(path) {
    String.capitalize(
        String.to_lower(List.to_string_using("", "", "::", path))
    );
}

function start(url) {
    match (url) {
    case { path : [], ... }: display("Hello");
    case { path : ["rest" | path], ... }: rest(topic_of_path(path));
    case ~{ path, ... }: display(topic_of_path(path));
    }

}

Server.start(
    Server.http,
    [ {resources: @static_include_directory("resources")}
      , {dispatch: start}
    ]
)
