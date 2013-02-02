import-plugin bootstrap-alert

@client
Alert = {{

  alert(dom:dom) =
    (%%alert.alert%%)(Dom.to_string(dom))

  close(dom:dom) =
    (%%alert.close%%)(Dom.to_string(dom))

}}
