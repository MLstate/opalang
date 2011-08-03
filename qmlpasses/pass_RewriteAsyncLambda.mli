(**
   This pass replaces the body of functions tagged with @async
   by a Scheduler.push(-> body)
*)

val process_code : val_:(string -> Ident.t) -> QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code -> QmlAst.annotmap * QmlAst.code
