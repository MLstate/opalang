/**
 * {1 Import standard classes of bootstrap css}
 *
 * see http://twitter.github.com/bootstrap/
 */
import stdlib.themes.bootstrap

/**
 * {1 Import templates}
 */
import stdlib.web.template

/**
 * {1 Database and database interaction}
 */

/**
 * Contents of the wiki.
 *
 * Pages which do not exist have content "This page is empty".
 * Note: By definition, pages stored in the database are always well-formed.
 */
db /wiki: stringmap(Template.default_content)
db /wiki[_] = Template.text("This page is empty. Double-click to edit.")


/**
 * Read the content associated to a topic from the database and return the
 * corresponding source code.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the source code for this
 * page. Otherwise, the source code for the default page.
 */
@publish load_source(topic)   = Template.to_source(Template.default, /wiki[topic])

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
@publish load_rendered(topic) = Template.to_xhtml(Template.default, /wiki[topic])

/**
 * Accept source code, save the corresponding document in the database.
 *
 * @param topic A topic (arbitrary string).
 * @param source Source code to store at this topic. If this source code
 * is syntactically valid, store the template datastructure
 * corresponding to its content [Template.content].
 * Otherwise, the source code is implicitly replaced by the document
 * representing this raw code and this document is saved in the database.
 *
 * @return In case of success, the xhtml for the page that has just been
 * saved. In case of failure, an error message.
 */
@publish save_source(topic, source) =
   match Template.try_parse(Template.default, source) with
    | ~{success}    ->
        do /wiki[topic] <- success;
        Template.to_xhtml(Template.default, success)
    | {failure = _} ->
        do /wiki[topic] <- Template.text(source);
        <>Error: {source}</>

/**
 * {1 User interface}
 */

/**
 * Set the user interface in edition mode.
 *
 * Load the source code for a topic, display an editable zone for this source code.
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
 * Save the source code for a topic (extracted from [#edit_content]),
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
       <textarea clas="xxlarge" rows="30" id=#edit_content onblur={_ -> save(topic)}></>
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
