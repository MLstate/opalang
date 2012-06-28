/**
 * {1 Import standard classes of bootstrap css}
 *
 * see http://twitter.github.com/bootstrap/
 */
import stdlib.themes.bootstrap

/**
 * {1 Import markdown syntax}
 */
import stdlib.tools.markdown

/**
 * {1 Database and database interaction}
 */

/**
 * Contents of the wiki.
 *
 * Pages which do not exist have content "This page is empty".
 */
database stringmap(string) /wiki
database /wiki[_] = "This page is empty. Double-click to edit."

/**
 * Read the content associated to a topic from the database and return the
 * corresponding Markdown source.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the source for this
 * page. Otherwise, the source for the default page.
 */
function load_source(topic) {
    /wiki[topic];
}

/**
 * Read the content associated to a topic from the database and return the
 * corresponding xhtml, ready to insert.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the xhtml for this
 * page. Otherwise, the xhtml for the default page.
 *
 * Note: This function does not perform any caching.
 * Note: This function is exposed because a user can ask the rendered content for any topic.
 */
exposed function load_rendered(topic) {
    source = load_source(topic);
    Markdown.xhtml_of_string(Markdown.default_options, source);
}

/**
 * Accept source and save the corresponding document in the database.
 *
 * @param topic A topic (arbitrary string).
 * @param source Markdown source to store at this topic.
 * @return The xhtml for the page that has just been saved.
 *
 * Note: This function is exposed because a user can save any content for any topic.
 */
exposed function save_source(topic, source) {
    /wiki[topic] <- source;
    load_rendered(topic);
}

/**
 * {1 User interface}
 */

/**
 * Set the user interface in edition mode.
 *
 * Load the Markdown source for a topic, display an editable zone
 * for this markdown.
 *
 * @param topic The topic to edit.
 */
function edit(topic) {
    Dom.set_value(#edit_content, load_source(topic));
    Dom.hide(#show_content);
    Dom.show(#edit_content);
    Dom.give_focus(#edit_content);
}

/**
 * Set the user interface in reading mode.
 *
 * Save the Markdown source for a topic (extracted from [#edit_content]),
 * display the rendered version.
 *
 * @param topic The topic to save.
 */
function save(topic) {
    content = save_source(topic, Dom.get_value(#edit_content));
    #show_content = content;
    Dom.hide(#edit_content);
    Dom.show(#show_content);
}

/**
 * Main user interface
 *
 * @param topic The topic being consulted
 * @return A resource, ready to be passed to a dispatcher.
 */
function display(topic) {
   xhtml =
     <div class="navbar navbar-fixed-top"><div class="navbar-inner"><div class="container"><div id=#logo></div></div></div></div>
     <div class="content container">
       <div class="page-header"><h1>About {topic}</></>
       <div class="well" id=#show_content ondblclick={function(_) { edit(topic) }}>{load_rendered(topic)}</>
       <textarea rows="30" id=#edit_content onblur={function(_) { save(topic) }}></>
     </div>;
   Resource.styled_page("About {topic}", ["/resources/css.css"], xhtml);
}

/**
 * {1 Main application}
 */

/**
 * Dispatch requests to the user interface
 *
 * Note: The empty request is dispatched as if it were "Hello".
 */
function start(url) {
  match (url) {
    case {path:[] ... } :
      display("Hello");
    case {~path ...} :
      display(String.capitalize(String.to_lower(String.concat("::", path))));
  }
}

/**
 * Start the wiki server
 */
Server.start(
    Server.http,
    /** Statically embed a bundle of resources */
    [ {resources: @static_include_directory("resources")}
      /** Launch the [start] dispatcher */
      , {dispatch: start}
    ]
);
