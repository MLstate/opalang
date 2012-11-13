import-plugin bootstrap-dropdown

@client
Dropdown = {{

  init(dom:dom) =
    (%%dropdown.init%%)(Dom.to_string(dom))

  toggle(dom:dom) =
    (%%dropdown.toggle%%)(Dom.to_string(dom))

}}
