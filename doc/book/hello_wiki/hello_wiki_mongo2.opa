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
  topic : string;
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

// Order by reverse _rev to get highest numbered _rev.
ord_collection = MongoCollection.orderby(wiki_collection,{some=Bson.opa2doc({_rev=-1})})

// Create a view so we can access just the revision field
type rev_only = {_rev:Bson.int32}
rev_field = MongoCollection.Fields.of_list([("_rev",1)])
rev_view = (MongoView.create(ord_collection, rev_field, true): Mongo.view(page,rev_only))

/**
 * Indexes aren't automatic in MongoDB apart from the non-removable _id index.
 * Since we're searching on _rev as well, we need a separate index.
 **/
_ = MongoCollection.create_index(wiki_collection, "db.wiki", Bson.opa2doc({topic=1; _rev=1}), 0)

/**
 * Retrieves a document from the database
 *
 * @param topic The id of the document to retrieve (arbitrary string)
 * @return content
 */
get_page(topic,rev_opt) =
  default = {~topic; _rev=0; content="This page is empty. Double-click to edit."}
  query = (match rev_opt with {some=rev} -> pageselect({~topic; _rev=rev}) | {none} -> pageselect({~topic}))
  match MongoCollection.find_one(ord_collection,query) with
  | {success=page} -> page
  | {failure={NotFound}} ->
     default
  | {~failure} ->
     do jlog("hello_wiki_mongo: failure={MongoCommon.string_of_failure(failure)}")
     default

/**
 * Return just the content for the given page.
 **/
get_content(topic, rev) = get_page(topic,rev).content

/**
 * Return the highest-numbered revision for a topic.
 **/
get_rev(topic) =
  match MongoView.find_one(rev_view,pageselect({~topic})) with
  | {success={_rev=rev}} -> rev
  | {failure={NotFound}} -> 0
  | {~failure} ->
     do jlog("hello_wiki_mongo(get_rev): failure={MongoCommon.string_of_failure(failure)}")
     0

/**
 * Read the content associated to a topic from the database and return the
 * corresponding Markdown source.
 *
 * @param topic A topic (arbitrary string).
 * @return If a page has been saved in for [topic], the source code for this
 * page. Otherwise, the source code for the default page.
 */
@publish load_source(topic, rev) =
  get_content(topic, rev)

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
@publish load_rendered(topic, rev) =
  Markdown.xhtml_of_string(Markdown.default_options, load_source(topic, rev))

/**
 * Accept source code, save the corresponding document in the database.
 *
 * @param topic A topic (arbitrary string).
 * @param source Markdown source to store at this topic.
 * @return In case of success, the xhtml for the page that has just been
 * saved. In case of failure, an error message.
 */
@publish save_source(topic, source, (revnum, maxrev)) =
  oldpage = get_page(topic,{some=revnum})
  if oldpage.content != source
  then
    result =
      if revnum == maxrev
      then
        page = {topic=topic; _rev=oldpage._rev+1; content=source}
        MongoCollection.insert_result(wiki_collection,page)
      else
        select = pageselect({~topic; _rev=(revnum:Bson.int32)})
        update = pageupdate({`$set`={content=source}})
        MongoCollection.update_result(MongoCollection.upsert(wiki_collection),select,update)
    if MongoCommon.is_error(result)
    then <>Error: {MongoCommon.pretty_of_result(result)}</>
    else load_rendered(topic,{some=revnum})
  else load_rendered(topic,{some=revnum})

/**
 * {1 User interface}
 */

current_revision(topic) =
  maxrev = get_rev(topic)
  match str_to_int(Dom.get_value(#wiki_rev_str)) with
  | {some=rev} -> (if rev < 1 || rev > maxrev then maxrev else rev, maxrev)
  | {none} -> (maxrev, maxrev)

/**
 * Set the user interface in edition mode.
 *
 * Load the Markdown source for a topic, display an editable zone
 * for this markdown.
 *
 * @param topic The topic to edit.
 */
edit(topic) =
  (revnum, _) = current_revision(topic)
  do Dom.set_value(#edit_content, load_source(topic,{some=revnum}))
  do Dom.hide(#show_content)
  do Dom.hide(#wiki_revision)
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
  content = save_source(topic, Dom.get_value(#edit_content), current_revision(topic))
  do Dom.transform([#show_content <- content])
  do set_revision0(topic)
  do Dom.hide(#edit_content)
  do Dom.show(#show_content)
  do Dom.show(#wiki_revision)
  void

/**
 * Convert a string into an integer option (why isn't this in stdlib?)
 **/
str_to_int(str) =
  string_to_integer = parser digits=([0-9]+) -> Int.of_string(Text.to_string(digits))
  Parser.try_parse(string_to_integer,str)

/**
 * Set the revision number on the displayed page.
 **/
set_revision(topic,rev) =
  revnum = Option.default(-1,str_to_int(rev))
  maxrev = get_rev(topic)
  revision = if revnum < 0 || revnum >= maxrev then "{maxrev}" else "{revnum} out of {maxrev}"
  Dom.transform([#wiki_rev_str <- rev,
                 #wiki_revision <- <div>Revision: {revision}</div>])

/**
 * Set the revision number for the given topic.
 **/
set_revision0(topic) =
  set_revision(topic,Dom.get_value(#wiki_rev_str))

/**
 * Update the displayed page with current text.
 **/
show(topic) =
  rev = Dom.get_value(#wiki_rev_str)
  do set_revision(topic,rev)
  Dom.transform([#show_content <- load_rendered(topic,str_to_int(rev))])

/**
 * Move forwards and backwards among the revisions.
 **/
inc(topic, inc) =
  (revnum, maxrev) = current_revision(topic)
  newrev = Int.min(Int.max(revnum + inc,1),maxrev)
  do Dom.set_value(#wiki_rev_str,"{newrev}")
  show(topic)

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
      <div>
        <input id="wiki_rev_str" class="wiki_input" type="text" value="" onnewline={_ -> show(topic)}></input>
        <a class="wiki_button" onclick={_ -> show(topic)} onready={_ -> show(topic)} href="#">Show</a>
        <a class="wiki_button" onclick={_ -> edit(topic)} href="#">Edit</a>
        <a class="wiki_button" onclick={_ -> inc(topic,-2147483648)} href="#">First</a>
        <a class="wiki_button" onclick={_ -> inc(topic,-10)} href="#">-10</a>
        <a class="wiki_button" onclick={_ -> inc(topic,-1)} href="#">Prev</a>
        <a class="wiki_button" onclick={_ -> inc(topic,1)} href="#">Next</a>
        <a class="wiki_button" onclick={_ -> inc(topic,10)} href="#">+10</a>
        <a class="wiki_button" onclick={_ -> inc(topic,2147483647)} href="#">Last</a>
      </div>
      <div class="well" id=#show_content ondblclick={_ -> edit(topic)}></>
      <div class="wiki_input" id="wiki_revision"></div>
      <textarea rows="30" id=#edit_content onblur={_ -> save(topic)}></>
    </div>)

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
