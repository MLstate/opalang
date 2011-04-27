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
    return js_void;
}

##register add_event_listener : Ace.t, string, ( -> void) -> void
##args(editor, kind, handler)
{
    editor.getSession().addEventListener(kind, function(){handler()});
    return js_void;
}

##register redo : Ace.t -> void
##args(editor)
{
    editor.getSession().getUndoManager().redo();
    return js_void;
}

##register undo : Ace.t -> void
##args(editor)
{
    editor.getSession().getUndoManager().undo();
    return js_void;
}

function setCookie(name,value)
{
    document.cookie=escape(name) + "=" + escape(value);
}

function getCookie(name)
{
var i,x,y,ARRcookies=document.cookie.split(";");
for (i=0;i<ARRcookies.length;i++)
{
  x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
  y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
  x=x.replace(/^\s+|\s+$/g,"");
  if (x==name)
    {
    return unescape(y);
    }
  }
}

##register save_input : -> void
##args()
{
    var input;
    var inputs = $('input[name!=""]');
    inputs.each(function(node){
            var val = this.value;
            if (val!="" && val!=undefined) setCookie("__opa_input_"+this.name, val);
        });
    return js_void;
}

##register load_input : -> void
##args()
{
    var input;
    var inputs = $('input[name!=""]');
    inputs.each(function(node){
            var val = this.value;
            var c = getCookie("__opa_input_"+this.name);
            if (val=="" && c != undefined) {
                this.value = c;
            }
        });
    return js_void;
}
