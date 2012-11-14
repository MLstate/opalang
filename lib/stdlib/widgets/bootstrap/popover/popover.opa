import-plugin bootstrap-popover

import stdlib.widgets.bootstrap.tooltip

/**
 * Example:
 *
 * import stdlib.themes.bootstrap
 * import stdlib.widgets.bootstrap.popover
 *
 * function page() {
 *
 *   <ul onready={function (_) Popover.init(Dom.select_raw_unsafe("*[rel=popover]"), Popover.default_options)}>
 *     <li><a class="btn" rel="popover" data-placement="top" data-content="Vivamus sagittis lacus vel augue laoreet rutrum faucibus." data-original-title="Popover on top">Popover on top</a></li>
 *     <li><a class="btn" rel="popover" data-placement="right" data-content="Vivamus sagittis lacus vel augue laoreet rutrum faucibus." data-original-title="Popover on right">Popover on right</a></li>
 *     <li><a class="btn" rel="popover" data-placement="bottom" data-content="Vivamus sagittis lacus vel augue laoreet rutrum faucibus." data-original-title="Popover on bottom">Popover on bottom</a></li>
 *     <li><a class="btn" rel="popover" data-placement="left" data-content="Vivamus sagittis lacus vel augue laoreet rutrum faucibus." data-original-title="Popover on left">Popover on left</a></li>
 *   </ul>
 *
 * }
 *
 * Server.start(Server.http, { title: "Popover", ~page })
 *
 */
@client
Popover = {{

  @both
  default_options= {
    animation = true
    html = false
    placement = {right}
    trigger = {click}
  } : Tooltip.options

  init(dom:dom, options:Tooltip.options) =
    placement = Tooltip.placement_to_string(options.placement)
    trigger = Tooltip.trigger_to_string(options.trigger)
    (%%popover.init%%)(Dom.to_string(dom), options.animation, options.html, placement, trigger)

  show(dom:dom) =
    (%%popover.show%%)(Dom.to_string(dom))

  hide(dom:dom) =
    (%%popover.hide%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%popover.toggle%%)(Dom.to_string(dom))

  destroy(dom:dom) =
    (%%popover.destroy%%)(Dom.to_string(dom))

}}
