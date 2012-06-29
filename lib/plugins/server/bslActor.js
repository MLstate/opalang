##extern-type Actor.t('ctx, 'msg)

##register make : 'state, \
('state, 'msg, option('ctx) -> option('state)), \
option(-> void), \
option('ctx), \
bool \
-> Actor.t('ctx, 'msg)
##args(state, handler, ondelete, ctx, concurrent)
{
  return new LocalChannel(state, null, handler, ctx, ondelete, null, concurrent);
}

##register post : option('ctx), 'msg , Actor.t('ctx, 'msg) -> void
##args(ctx, msg, actor)
{
  actor.send(null, msg, ctx);
  return js_void;
}

/* Hack for non cps client ****************************** */
##module SynchronousCell
  /**
   * Synchronous call a cell used for non cps mode.
   */
  ##register llcall : Actor.t('ctx, 'cmsg), 'msg, ('c, 'msg -> 'b) -> 'result
  ##args(actor, message, onmsg)
  {
      return actor.call_no_cps(message, null, null, onmsg);
  }

##endmodule
