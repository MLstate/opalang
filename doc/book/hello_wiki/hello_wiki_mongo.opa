/**
 * {1 Import the high-level MongoDB API module}
 *
 * For the low-level API see the package stdlib.apis.mongo
 */
import stdlib.apis.{mongo}

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
 * The basic info. about the database and table location.
 */
type page = {
  _id : string;
  _rev : Bson.int32;
  content : string;
}

/**
 * We work at level 1, run-time type-checked storage of a collection of OPA values.
 *
 * The Mongo.pkg type provides convenience functions for building select and update documents.
 **/
(wiki_collection,wiki_pkg) = (MongoCollection.openpkgfatal("default","db","wiki"):Mongo.pkg(page))
pageselect(v) = wiki_pkg.select(Bson.opa2doc(v))
pageupdate(v) = wiki_pkg.update(Bson.opa2doc(v))

/**
 * Indexes aren't automatic in MongoDB apart from the non-removable _id index.
 * Since we're searching on _rev as well, we need a separate index.
 **/
_ = MongoCollection.create_index(wiki_collection, "db.wiki", Bson.opa2doc({_id=1; _rev=1}), 0)

/**
 * Retrieves a document from the database
 *
 * @param docid The id of the document to retrieve (arbitrary string)
 * @return content
 */
get_content(docid) =
  default = "This page is empty. Double-click to edit."
  extract_content(record:page) = record.content
  // Order by reverse _rev to get highest numbered _rev.
  orderby = {some=Bson.opa2doc({_rev=-1})}
  match MongoCollection.find_one(MongoCollection.orderby(wiki_collection,orderby),pageselect({_id=docid})) with
  | {success=page} -> extract_content(page)
  | {failure={NotFound}} ->
     default
  | {~failure} ->
     do jlog("hello_wiki_mongo: failure={MongoCommon.string_of_failure(failure)}")
     default

/**
 * Read the content associated to a topic from the database and return the
 * corresponding Markdown source.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the source code for this
 * page. Otherwise, the source code for the default page.
 */
@publish load_source(topic) =
  get_content(topic)

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
@publish load_rendered(topic) =
  Markdown.xhtml_of_string(Markdown.default_options, load_source(topic))

/**
 * Accept source code, save the corresponding document in the database.
 *
 * @param topic A topic (arbitrary string).
 * @param source Markdown source to store at this topic.
 * @return In case of success, the xhtml for the page that has just been
 * saved. In case of failure, an error message.
 */
@publish save_source(topic, source) =
  select = pageselect({_id=topic})
  update = pageupdate({`$set`={content=source}; `$inc`={_rev=(1:Bson.int32)}})
  // Upsert this so we create it if it isn't there
  result = MongoCollection.update_result(MongoCollection.upsert(wiki_collection),select,update)
  if MongoCommon.is_error(result)
  then <>Error: {MongoCommon.pretty_of_result(result)}</>
  else load_rendered(topic)

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
     <div class="topbar"><div class="fill"><div class="container">
       <div id=#logo></div>
     </div></div></div>
     <div class="content container">
       <div class="page-header"><h1>About {topic}</></>
       <div class="well" id=#show_content ondblclick={_ -> edit(topic)}>
         {load_rendered(topic)}
       </>
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
