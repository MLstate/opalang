import-plugin bootstrap-tooltip

type Tooltip.placement = {top} / {bottom} / {left} / {right}

type Tooltip.trigger = {click} / {hover} / {focus} / {manual}

type Tooltip.options = {
  animation: bool
  html: bool
  placement: Tooltip.placement
  trigger: Tooltip.trigger
}

@client
Tooltip = {{

  @both
  default_options= {
    animation = true
    html = false
    placement = {top}
    trigger = {hover}
  } : Tooltip.options

  placement_to_string(p:Tooltip.placement) =
    match p
    {top} -> "top"
    {bottom} -> "bottom"
    {left} -> "left"
    {right} -> "right"

  trigger_to_string(t:Tooltip.trigger) =
    match t
    {click} -> "click"
    {hover} -> "hover"
    {focus} -> "focus"
    {manual} -> "manual"

  init(dom:dom, options:Tooltip.options) =
    placement = placement_to_string(options.placement)
    trigger = trigger_to_string(options.trigger)
    (%%tooltip.init%%)(Dom.to_string(dom), options.animation, options.html, placement, trigger)

  show(dom:dom) =
    (%%tooltip.show%%)(Dom.to_string(dom))

  hide(dom:dom) =
    (%%tooltip.hide%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%tooltip.toggle%%)(Dom.to_string(dom))

  destroy(dom:dom) =
    (%%tooltip.destroy%%)(Dom.to_string(dom))

}}
