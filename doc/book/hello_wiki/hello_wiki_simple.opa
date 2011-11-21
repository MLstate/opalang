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
db /wiki: stringmap(string)
db /wiki[_] = "This page is empty. Double-click to edit."

/**
 * Read the content associated to a topic from the database and return the
 * corresponding Markdown source.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the source for this
 * page. Otherwise, the source for the default page.
 */
load_source(topic) =
  /wiki[topic]

/**
 * Read the content associated to a topic from the database and return the
 * corresponding xhtml, ready to insert.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the xhtml for this
 * page. Otherwise, the xhtml for the default page.
 *
 * Note: This function does not perform any caching.
 */
load_rendered(topic) =
  source = load_source(topic)
  Markdown.xhtml_of_string(Markdown.default_options, source)

/**
 * Accept source and save the corresponding document in the database.
 *
 * @param topic A topic (arbitrary string).
 * @param source Markdown source to store at this topic.
 * @return The xhtml for the page that has just been saved.
 */
save_source(topic, source) =
  do /wiki[topic] <- source
  load_rendered(topic)

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
edit(topic) =
   do Dom.set_value(#edit_content, load_source(topic))
   do Dom.hide(#show_content)
   do Dom.show(#edit_content)
   do Dom.give_focus(#edit_content)
   void

/**
 * Set the user interface in reading mode.
 *
 * Save the Markdown source for a topic (extracted from [#edit_content]),
 * display the rendered version.
 *
 * @param topic The topic to save.
 */
save(topic) =
   content = save_source(topic, Dom.get_value(#edit_content))
   do Dom.transform([#show_content <- content])
   do Dom.hide(#edit_content)
   do Dom.show(#show_content)
   void

/**
 * Main user interface
 *
 * @param topic The topic being consulted
 * @return A resource, ready to be passed to a dispatcher.
 */
display(topic) =
   Resource.styled_page("About {topic}", ["/resources/css.css"],
     <div class="topbar"><div class="fill"><div class="container"><div id=#logo></div></div></div></div>
     <div class="content container">
       <div class="page-header"><h1>About {topic}</></>
       <div class="well" id=#show_content ondblclick={_ -> edit(topic)}>{load_rendered(topic)}</>
       <textarea rows="30" id=#edit_content onblur={_ -> save(topic)}></>
     </div>
   )

/**
 * {1 Main application}
 */

/**
 * Dispatch requests to the user interface
 *
 * Note: The empty request is dispatched as if it were "Hello".
 */
start =
   | {path = [] ... } ->
       display("Hello")
   | {~path ...}      ->
       display(String.capitalize(String.to_lower(String.concat("::", path))))

/**
 * Statically embed a bundle of resources
 */
server = Server.of_bundle([@static_include_directory("resources")])

/**
 * Launch the [start] dispatcher
 */
server = Server.simple_dispatch(start)
