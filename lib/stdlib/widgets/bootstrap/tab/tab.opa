import-plugin bootstrap-tab

@client
Tab = {{

  show(dom:dom) =
    (%%tab.show%%)(Dom.to_string(dom))

}}
