import-plugin bootstrap-popover

import stdlib.widgets.bootstrap.tooltip

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
