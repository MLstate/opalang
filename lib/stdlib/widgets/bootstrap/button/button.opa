import-plugin bootstrap-button

@client
Button = {{

  init(dom:dom) =
    (%%button.init%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%button.toggle%%)(Dom.to_string(dom))

  loading(dom:dom) =
    (%%button.loading%%)(Dom.to_string(dom))

  reset(dom:dom) =
    (%%button.reset%%)(Dom.to_string(dom))

  button(dom:dom, s:string) =
    (%%button.button%%)(Dom.to_string(dom), s)

}}
