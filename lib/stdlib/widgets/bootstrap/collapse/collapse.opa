import-plugin bootstrap-collapse

import stdlib.widgets.bootstrap.transition

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
