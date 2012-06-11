
type ServerArg.args = list(string)

type ServerArg.instruction('a) = option(CommandLine.change_state('a))

ServerArg = {{

  @private sa_args = ServerReference.create(%%BslSys.argv%%():list(string))
  get_argv() = ServerReference.get(sa_args)
  set_argv(args) = ServerReference.set(sa_args,args)

  bool_of_string(str:string) : option(bool) = Parser.try_parse(Rule.bool, str)
  int_of_string(str:string) : option(int) = Parser.try_parse(Rule.integer, str)
  float_of_string(str:string) : option(float) = Parser.try_parse(Rule.float, str)

  int(args) =
    match args with
    | [x|args] -> (int_of_string(x), [], args)
    | [] -> (none, [], [])

  string(args) =
    match args with
    | [x|args] ->
       if String.length(x) == 0 || not(String.has_prefix("-",x))
       then ({some=x},[],args)
       else (none,[],[])
    | _ -> (none, [], [])

  anystring(args) =
    match args with
    | [x|args] -> ({some=x}, [], args)
    | [] -> (none, [], [])

  float(args) =
    match args with
    | [x|args] -> (float_of_string(x), [], args)
    | [] -> (none, [], [])

  bool(args) =
    match args with
    | [x|args] -> (bool_of_string(x), [], args)
    | [] -> (none, [], [])

  unit(args) = ({some={}}, [], args)

  stringset(list, args) =
    match args with
    | [x|args] -> (List.assoc(x,list), [], args)
    | [] -> (none, [], [])

  list(separator,parse,args) =
    match args with
    | [] -> ({some=[]}, [], [])
    | [x|args] ->
       match
         List.fold((x, res ->
                    match res with
                    | {some=l} -> 
                       match parse([x]) with
                       | ({some=x}, [], []) -> {some=[x|l]}
                       | _ -> none
                       end
               | {none} -> none),String.explode(separator, x),{some=[]})
      with
      | {none} -> (none, [], args)
      | {some=l} -> ({some=List.rev(l)}, [], args)
      end

  option(parse,args) =
    match parse(args) with
    | ({some=x}, skipped, args) -> ({some={some=x}}, skipped, args)
    | _ -> ({some=none}, [], args)

  pair(pa,pb,args) =
    match pa(args) with
    | ({none}, skipped, args) -> (none, skipped, args)
    | ({some=x}, skipped, args) ->
      match pb(args) with
      | ({none}, skipped2, args) -> (none, List.append(skipped,skipped2), args)
      | ({some=y}, skipped2, args) -> ({some=(x,y)}, List.append(skipped,skipped2), args)
      end

  check(parse,f,args) =
    match parse(args) with
    | ({none}, skipped, args) -> (none, skipped, args)
    | ({some=x}, skipped, args2) ->
        if f(x)
        then ({some=x}, skipped, args2)
        else (none, [], args)

  keep(parse,args) =
    (res, _skipped, _args) = parse(args)
    (res, [], args)

  wrap(param,f,args) =
    match param(args) with
    | ({some=x}, skipped, args) -> ({some=f(x)}, skipped, args)
    | ({none}, skipped, args) -> (none, skipped, args)

  wrap_opt(param,f,args) =
    match param(args) with
    | ({none}, skipped, args) -> (none, skipped, args)
    | ({some=x}, skipped, args2) ->
      match f(x) with
      | {none} -> (none, [], args)
      | res -> (res, skipped, args2)
      end

  func(param,f,acc,args) = wrap(param,f(acc),args)

  func_opt(param,f,acc,args) = wrap_opt(param,f(acc,_),args)

  skip(args) =
    match args with
    | [x|args] -> ({some={}}, [x], args)
    | [] -> (none, [], [])

  skip_all(args) = ({some={}}, args, [])

  fold(effect,acc,args) =
    match args with
    | [] -> ({some=acc}, [], [])
    | args ->
        match effect(acc,args) with
        | ({some=acc}, skipped, args) ->
            (acc, skipped2, args) = fold(effect,acc,args)
            (acc, List.append(skipped,skipped2), args)
        | _ -> ({some=acc}, [], args)
        end

  fold_until(str,effect,acc,args) =
    fold((acc, args ->
          match args with
          | [] -> ({some=acc},[],[])
          | [x|_] -> if x == str then ({some=acc},args,[]) else effect(acc,args)),acc,args)

  fold_all(effect,acc,remainings_args) =
    match remainings_args with
    | [] -> ({some=acc}, [], [])
    | [arg|rest] as args ->
        match effect(acc,args) with
        | ({some=acc}, skipped, args) ->
            (acc, skipped2, args) = fold_all(effect,acc,args)
            (acc, List.append(skipped,skipped2), args)
        | ({none}, skipped, _args) ->
            (acc, skipped2, args) = fold_all(effect,acc,rest)
            (acc, [arg|List.append(skipped,skipped2)], args)

  skip_str(str,effect,args) =
    match args with
    | [] -> ({some=none}, [], [])
    | [x|r] ->
        if x == str
        then option(effect,r)
        else
          if List.mem(str,args)
          then (none, [], args)
          else option(effect,args)

  push(str,args) = ({some={}}, [str], args)

  make_arg_parser(names:list(string), param_doc:string, doc:string,
                  initialize:'a -> CommandLine.change_state('a),
                  step:'a, string -> ServerArg.instruction('a))  =
    rec parse_params(state:CommandLine.change_state('a), args, used) : (option('a), ServerArg.args, ServerArg.args) =
      match state with
      | {no_params=x} -> /*Nothing to do, so job finished*/
          ({some=x}, [], args)
      | {opt_params=x} ->
         match args with
         | [] ->             /*No args left, so job finished*/
             ({some=x}, [], args)
         | [h|t] ->
             match step(x,h) with
             | {none} ->                     /*Optional arg doesn't parse, so job finished*/
                 ({some=x}, [], args)
             | {some={no_params=x}} ->  /*This was the last arg, job finished*/
                 ({some=x}, [], t)
             | {some=i} ->                   /*Otherwise, continue job*/
                 parse_params(i,t,[h|used])
             end
         end
      | {params=x} ->
         match args with
         | [] ->                           /*No args left, this is a failure*/
             ({none}, used, [])
         | [h|t] ->
            match step(x,h) with
            | {none} ->                     /*Arg doesn't parse, this is a failure*/
                ({none}, used, args)
            | {some={no_params=x}} ->
                ({some=x}, [], t)
            | {some=i} ->                   /*So far, so good*/
                parse_params(i,t,[h|used])
            end
         end
    parse(state,args) = parse_params(initialize(state),args,[])
    (names, parse, param_doc, doc)

  // Primitive version of Caml Format output...
  format(s,sep,width) =
    frags = String.explode(" ",s)
    rec join(frags, slen, sl:list(list(string))) =
      match (frags, sl) with
      | ([f|frags],[]) -> join(frags, String.length(f), [[f]])
      | ([f|frags],[sl|sll]) ->
        flen = String.length(f) + 1
        if slen + flen > width
        then join(frags, flen-1, [[f]|[sl|sll]])
        else join(frags, slen+flen, [[f|sl]|sll])
      | ([],sll) -> String.concat(sep, List.map((l -> String.concat(" ",List.rev(l))),List.rev(sll)))
    sep^join(frags, 0, [])

  doc_string(title,speclist) =
    di = "\n                "
    String.concat("\n",
                  List.append([String.capitalize(title)^":"],
                               List.map(((names,_,params_doc,doc) ->
                                         String.concat("",["    ",String.concat(", ",names)," ",params_doc,format(doc,di,60)])),
                                        speclist)))^"\n\n"

  make_parser(final, nohelp, title, speclist, acc0, args0) =
    final = Option.default(false,final)
    nohelp = Option.default(false,nohelp)
    rec do_args((acc,rev_args),l) =
      match l with
      | [] -> ({some=acc}, List.rev(rev_args), [])
      | [arg|args] ->
          rec do_specs(specs) =
            match specs with
            | [(names,effect,params_doc,_doc)|specs] ->
                if List.mem(arg,names)
                then
                  match effect(acc,args) with
                  | ({some=acc}, skipped_args, args) ->
                      do_args((acc, List.rev_append(skipped_args,rev_args)), args)
                  | _ ->
                      do prerr("Invalid parameter for option {arg}, in {title}. Syntax:\n    {arg} {params_doc}\n")
                      @fail //exit(-1)
                  end
                else
                  do_specs(specs)
            | [] ->
                if not(nohelp) && (arg == "--help" || arg == "-help" || arg == "-h" || arg == "-?")
                then
                  do prerr("{doc_string(title,speclist)}")
                  if final
                  then @fail //exit(-1)
                  else ({some=acc0},args0,[])
                else
                  do_args((acc, [arg|rev_args]),args)
            end
          do_specs(speclist)
    do_args((acc0,[]),args0)

  sa_filter(acc,parse) =
    args = get_argv()
    match parse(acc,args) with
    | ({none}, _, _) -> acc
    | ({some=acc}, skipped_args, args) ->
        do set_argv(List.append(skipped_args,args))
        acc

  filter(topic,args,init) =
    my_parser = make_parser(none,none,topic,args,_,_)
    sa_filter(init,my_parser)

  anonymous_filter(func,init) =
    param_parser = func_opt(anystring,func,_,_)
    param_parser = fold_all(param_parser,_,_)
    sa_filter(init,param_parser)

}}


