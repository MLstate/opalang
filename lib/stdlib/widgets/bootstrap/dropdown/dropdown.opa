import-plugin bootstrap-dropdown

/**
 * {2 Example:}
 *
 * {[
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.dropdown
 *
 * function page() {
 *
 *   <div class="dropdown">
 *     <a class="dropdown-toggle" data-toggle="dropdown" href="#">Dropdown trigger</a>
 *     <ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">
 *       <li>hello</li>
 *       <li>world</li>
 *     </ul>
 *   </div>
 *   <div class="dropdown">
 *     <a class="dropdown-toggle" id="dLabel" role="button" data-toggle="dropdown" data-target="#" href="/page.html">
 *       Dropdown
 *       <b class="caret"></b>
 *     </a>
 *     <ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">
 *       <li>hello</li>
 *       <li>world</li>
 *     </ul>
 *   </div>
 *
 * }
 *
 * Server.start(Server.http, { title: "Dropdown", ~page })
 *
 * }
 *
 */
@client
Dropdown = {{

  init(dom:dom) =
    (%%dropdown.init%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%dropdown.toggle%%)(Dom.to_string(dom))

}}
