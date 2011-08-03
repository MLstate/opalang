##extern-type Dom.private.element

##register create_draggable : Dom.private.element -> void
##args(element)
{
    element.draggable();
}

##register destroy_draggable : Dom.private.element -> void
##args(element)
{
    element.draggable("destroy");
}

##register set_option_string \ bsl_set_option_draggable: Dom.private.element,string,string -> void
##register set_option_int \ bsl_set_option_draggable: Dom.private.element,string,int -> void
##register set_option_bool \ bsl_set_option_draggable: Dom.private.element,string,bool -> void
function bsl_set_option_draggable(element,option,value)
{
    element.draggable("option",option,value);
}

##register set_option_array : Dom.private.element,string,int,int -> void
##args(element,option,value1,value2)
{
    element.draggable("option",option,[value1,value2]);
}

##register enable_draggable : bool, Dom.private.element -> void
##args(set,element)
{
    if(set){
        element.draggable("enable");
    } else {
        element.draggable("disable");
    }
}

##register create_droppable : Dom.private.element,string -> void
##args(element,hover_class)
{
    element.droppable();
    element.droppable("option","hoverClass",hover_class);
}

##register destroy_droppable : Dom.private.element -> void
##args(element)
{
    element.droppable("destroy");
}

##register enable_droppable : bool, Dom.private.element -> void
##args(set,element)
{
    if(set){
        element.droppable("enable");
    } else {
        element.droppable("disable");
    }
}

##register assign : string, Dom.private.element -> void
##args(draggable, droppable)
{
    droppable.droppable("option","accept",draggable);
}
