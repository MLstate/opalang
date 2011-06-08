/**
 * For documentation please see [ace.opa]
 */
##extern-type [opaname] Ace.t = object

##register edit : string -> Ace.t
##args(id)
{
  var r = ace.edit(id);
  return r;
}

##register set_mode : Ace.t, string -> bool
##args(editor, mode)
{
  var M = require("ace/mode/"+mode);
  if(M == undefined) return false;
  M = M.Mode;
  if(M == undefined) return false;
  editor.getSession().setMode(new M());
  return true;
}

##register get_content : Ace.t -> string
##args(editor)
{
    return editor.getSession().getValue();
}

##register set_content : Ace.t, string -> void
##args(editor, content)
{
    editor.getSession().setValue(content);
}

##register add_event_listener : Ace.t, string, ( -> 'void) -> void
##args(editor, kind, handler)
{
    editor.getSession().addEventListener(kind, handler);
}

##register redo : Ace.t -> void
##args(editor)
{
    editor.getSession().getUndoManager().redo();
}

##register undo : Ace.t -> void
##args(editor)
{
    editor.getSession().getUndoManager().undo();
}

##register read_only : Ace.t, bool -> void
##args(editor, b)
{
    editor.setReadOnly(b);
}

##register move_cursor : Ace.t, int, int -> void
##args(editor, col, line)
{
    editor.getSelection().moveCursorToScreen(col, line);
}
