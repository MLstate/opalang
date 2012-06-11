##extern-type Actor.t('msg, 'ctx)

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

##register [cps-bypass] make_cps : 'state, \
('state, 'msg, option('ctx), continuation(opa[option('state)]) -> void), \
option(continuation(opa[void]) -> void), \
option('ctx), \
bool, \
continuation(Actor.t('ctx, 'msg)) -> void
##args(state, handler, ondelete, ctx, concurrent, k)
{
  var h = function(st, msg, ctx, fk){ handler(st, msg, ctx, new Continuation(fk)) };
  var d = opa_cps_callback_to_js_callback0(ondelete);
  var a = new LocalChannel(state, null, h, ctx, d, null, concurrent);
  return_(k, a);
}

##register post : option('ctx), 'msg , Actor.t('ctx, 'msg) -> void
##args(ctx, msg, actor)
{
  actor.send(null, msg, ctx);
  return js_void;
}
