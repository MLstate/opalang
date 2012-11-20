import-plugin meteor-spark

type Context.t = external

client module Spark {

    render          = %% Spark.render %%
    function render_f(htmlFunc) {
        function f() { render(htmlFunc) }
        f
    }
    isolate         = %% Spark.isolate %%
    labelBranch     = %% Spark.labelBranch %%
    list            = %% Spark.list %%

    function replace_f(position, item_f){
        Dom.to_selection(
            %% Spark.replace_f %%(Dom.of_selection(position), { function() item_f() } )
        )
    }
}

client module Context {

    getId           = %% Context.getId %%
    get             = %% Context.get %%
    onInvalidate    = %% Context.onInvalidate %%
    invalidate      = %% Context.invalidate %%
    empty           = %% Context.empty %%
}
