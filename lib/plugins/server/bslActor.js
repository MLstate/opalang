/** @externType Actor.t('ctx, 'msg) */

/**
 * @register {'state, \
              ('state, 'msg, opa[option('ctx)] -> opa[option('state)]), \
              opa[option(-> void)], \
              opa[option('ctx)], \
              bool \
              -> Actor.t('ctx, 'msg)}
 */
function make(state, handler, ondelete, ctx, concurrent) {
  return new LocalChannel(state, null, handler, ctx, option2js(ondelete), null, concurrent);
}

/**
 * @register {opa[option('ctx)], 'msg , Actor.t('ctx, 'msg) -> void}
 */
function post(ctx, msg, actor) {
  actor.send(null, msg, ctx);
  return js_void;
}

/* Hack for non cps client ****************************** */
/** @module SynchronousCell */
  /**
   * Synchronous call a cell used for non cps mode.
   * @register {Actor.t('ctx, 'cmsg), 'msg, ('c, 'msg -> 'b) -> 'result} llcall
   */
  function sync_cell_llcall(actor, message, onmsg) {
      return actor.call_no_cps(message, null, null, onmsg);
  }

/** @endModule */
