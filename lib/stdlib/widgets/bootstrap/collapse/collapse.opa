import-plugin bootstrap-collapse

import stdlib.widgets.bootstrap.transition

/**
 * Example:
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.collapse
 *
 * function page() {
 *
 *   <div class="accordion" id="accordion">
 *     <div class="accordion-group">
 *       <div class="accordion-heading">
 *         <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion" href="#collapseOne">
 *           Collapsible Group Item #1
 *         </a>
 *       </div>
 *       <div id="collapseOne" class="accordion-body collapse in">
 *         <div class="accordion-inner">
 *           Anim pariatur cliche...
 *         </div>
 *       </div>
 *     </div>
 *     <div class="accordion-group">
 *       <div class="accordion-heading">
 *         <a class="accordion-toggle" data-toggle="collapse" data-parent="#accordion" href="#collapseTwo">
 *           Collapsible Group Item #2
 *         </a>
 *       </div>
 *       <div id="collapseTwo" class="accordion-body collapse">
 *         <div class="accordion-inner">
 *           Anim pariatur cliche...
 *         </div>
 *       </div>
 *     </div>
 *   </div>
 *   <div id=#cl1 class="collapse in">Some Text</div>
 *   <button type="button" class="btn btn-danger" data-toggle="collapse" data-target="#cl1">
 *     Collapse
 *   </button>
 *
 *   <div onclick={function (_) Collapse.toggle(#cl1)}>Toggle</div>
 *   <div onclick={function (_) Collapse.show(#cl1)}>Show</div>
 *   <div onclick={function (_) Collapse.hide(#cl1)}>Hide</div>
 *
 * }
 *
 * Server.start(Server.http, { title: "Collapse", ~page })
 *
 */
@client
Collapse = {{

  init(dom:dom) =
    (%%collapse.init%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%collapse.toggle%%)(Dom.to_string(dom))

  show(dom:dom) =
    (%%collapse.show%%)(Dom.to_string(dom))

  hide(dom:dom) =
    (%%collapse.hide%%)(Dom.to_string(dom))

}}
