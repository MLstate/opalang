/** @externType Context.t */

/**
 * @register { -> Context.t }
 */
function get() {
    return Meteor.deps.Context.current;
}

/**
 * @register { Context.t -> int }
 */
function getId(context) {
    if(context == null){
        return -1
    }else{
        return context.id
    }
}

/**
 * @register { Context.t, (-> void) -> void }
 */
function onInvalidate(context, callback) {
    context.onInvalidate(callback);
}

/**
 * @register { Context.t -> void }
 */
function invalidate(context) {
    context.invalidate();
}

/**
 * @register { Context.t }
 */
 var empty = null
