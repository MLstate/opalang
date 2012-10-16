type options = Opx2jsOptions.t

type ('env, 'env2) pass = (options, options, 'env, 'env2) PassHandler.pass

val pass_Welcome : (unit, options, unit, unit) PassHandler.pass

val pass_CheckOptions : (unit, unit) pass

type env

val pass_LoadEnvironment : ((options, env) PassHandler.one_env -> int) -> (unit, int) pass

val pass_NodeJsPluginCompilation : (env, env) pass

val pass_NodeJsPluginGeneration : (env, int) pass
