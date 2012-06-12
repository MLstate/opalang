/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/
/*
   @author Frederic Ye
*/
import stdlib.core.date

/* An entry priority */
type Logger.priority =
  / {Emergency}
  / {Alert}
  / {Critical}
  / {Error}
  / {Warning}
  / {Notice}
  / {Info}
  / {Debug}

type Logger.date = external

type Logger.entry = {
  time : Logger.date;         /* time of log */
  priority : Logger.priority; /* priority */
  message : string;           /* message */
  color : Ansi.color;         /* color if printed in a tty */
}

type Logger.out_channel = external

type Logger.destination =
  / {Blackhole}                  /* a blackhole where nothing will be output */
  / {Ram}                        /* keep the log list in RAM */
  / {Channel:Logger.out_channel} /* write log to a channel */

type Logger.named_destination = (string, Logger.destination) // Lazy.t???

type Logger.t = {
  destinations : list(Logger.named_destination); /* list of destinations */
  ram : reference(list(Logger.entry));           /* list of entry to keep in ram */
  filter : int;                                  /* level after this filter number will not be produced */
  lasttime : reference(Logger.date);             /* last time a log was output */
}

type Logger.log_opts = {
  verbose : int;
  display : bool;
  path : string;
  date : bool;
  rotate : option(string);
  rotate_interval : option(int);
}

//@server_private
Logger = {{

  /* Convert an entry priority to a string */
  priority_to_string(p) =
    match p with
    | {Emergency} -> "emerg"
    | {Alert} -> "alert"
    | {Critical} -> "crit"
    | {Error} -> "error"
    | {Warning} -> "warn"
    | {Notice} -> "notice"
    | {Info} -> "info"
    | {Debug} -> "debug"

  /* Convert an entry priority to a level in order to filter more easily */
  priority_to_level(p) =
    match p with
    | {Emergency} -> 0
    | {Alert} -> 1
    | {Critical} -> 2
    | {Error} -> 3
    | {Warning} -> 4
    | {Notice} -> 5
    | {Info} -> 6
    | {Debug} -> 7

  /* Some references for rotating logs
     TODO: should be improved later */
  logs_path = ServerReference.create({none}:option(string)) /* logs_path option */
  with_rotatelogs = ServerReference.create({none}:option((string,int))) /* (rotatelogs_bin_path * days) option  */
  date_logs = ServerReference.create(false)

  make_file_destination(file) =
    (file,
     match %%BslLogger.open_out%%(file) with
     | {some=oc} -> {Channel=oc}
     | {none} -> {Blackhole}) /* FIXME: Display a message to warn that this file can't be accessed */
  make_piped_destination(cmd) =
    (cmd,
     match %%BslLogger.open_pipe%%(cmd) with
     | {some=oc} -> {Channel=oc}
     | {none} -> {Blackhole}) /* FIXME: Display a message to warn that this file can't be accessed */

  make_rotating_destination(days, filename) =
    lp = Option.default("",ServerReference.get(logs_path))
    match ServerReference.get(with_rotatelogs) with
    | {some=(rpath, rdays)} ->
        // We here multiply by 86400 because rotatelogs rotationtime is in seconds
        rt = Option.default(rdays,days) * 86400
        pipe = "{rpath} -l \"{lp}{filename}%Y.%m.%d.log\" {rt}"
        make_piped_destination(pipe)
    | _ ->
        if ServerReference.get(date_logs)
        then
          log_suffix = %%BslLogger.log_suffix%%()
  	  file = "{lp}{filename}{log_suffix}.log"
  	  make_file_destination(file)
        else
  	  file = "{lp}{filename}.log"
  	  make_file_destination(file)

  /* Checks if the destination is a tty */
  is_tty(p) =
    match p with
    | {Blackhole} -> false
    | {Ram} -> false
    | {Channel=c} -> %%BslLogger.is_tty%%(c)

  default_filer_level = 6

  empty_logger() = {
    destinations=[]:list(Logger.named_destination);
    ram=ServerReference.create([]:list(Logger.entry));
    filter=default_filer_level;
    lasttime=ServerReference.create(%%BslLogger.now%%());
  }

  make_logger(dsts,filter) = {
    destinations=dsts;
    ram=ServerReference.create([]:list(Logger.entry));
    filter=filter;
    lasttime=ServerReference.create(%%BslLogger.now%%());
  }

  add_destination(logger,dst) = {logger with destinations=[dst|logger.destinations]}

  clear_destinations(logger) = {logger with destinations=[]:list(Logger.named_destination)}

  set_filter(logger,filter) = {logger with ~filter}

  get_ram(logger) : list(Logger.entry) = ServerReference.get(logger.ram)
  get_filter(logger) = logger.filter

  no_logger = empty_logger()

  /* Create a reference to a logger that will write to stderr */
  default_logger =
    logger = empty_logger()
    ServerReference.create({logger with destinations=[("STDERR", {Channel=%%BslLogger.get_stderr%%()})]})

  get_default_logger_destinations() = List.map((d -> d.f1),ServerReference.get(default_logger).destinations)

  set_default_logger(logger) = ServerReference.set(default_logger,logger)

  // TODO: emulate lazy creation of log files with options and refs.
  /* Create a reference to a logger that will write to "access.log" */
  access_logger =
    logger = empty_logger()
    dst = make_rotating_destination({none},"access")
    ServerReference.create({logger with destinations=[dst]})

  get_access_logger_destinations() = List.map((d -> d.f1),ServerReference.get(access_logger).destinations)

  set_access_logger(logger) = ServerReference.set(access_logger,logger)

  /* Create a reference to a logger that will write to "error.log" */
  error_logger =
    logger = empty_logger()
    dst = make_rotating_destination({none},"error")
    ServerReference.create({logger with destinations=[dst]})

  get_error_logger_destinations() = List.map((d -> d.f1),ServerReference.get(error_logger).destinations)

  set_error_logger(logger) = ServerReference.set(error_logger,logger)

  force_colour = #<Ifstatic: MLSTATE_FORCE_COLOR 1> true #<Else> false #<End>

  /* Convert a log entry into a string *
     @param long display a long message (date + priority)
     @dest the log destination
     @entry the log entry */
  string_of_entry(long,dest,entry) =
    message =
      if is_tty(dest) || force_colour
      then Ansi.print(entry.color,entry.message)
      else entry.message
    if #<Ifstatic:TESTING> false #<Else> long #<End>
    then
      time = %%BslLogger.log_time%%(entry.time)
      "{time} [{priority_to_string(entry.priority)}]: {message}\n"
    else
      message^"\n"

  make_entry(priority,color,message) = ~{ time=%%BslLogger.now%%(); priority; message; color }

  do_log(logger,long,priority,color,s) =
    if priority_to_level(priority) > logger.filter
    then {}
    else
      entry = make_entry(priority,color,s)
      log_entry(dst) =
  	sentry = string_of_entry(long,dst,entry)
  	match dst with
  	| {Blackhole} -> {}
  	| {Ram} -> ServerReference.set(logger.ram,[entry|ServerReference.get(logger.ram)])
  	| {Channel=oc} -> %%BslLogger.output%%(oc,sentry)
        end
  	do ServerReference.set(logger.lasttime,entry.time)
      #<Ifstatic:SHOW_LOGS> /* If SHOW_LOGS, only log one time in stderr */
        dst = {Channel=%%BslLogger.get_stderr%%()}
        log_entry(dst)
      #<Else>
        List.iter(((_, dst) -> log_entry(dst)),logger.destinations)
      #<End>

  log(logger, priority, color, s) =
    logger = Option.default(ServerReference.get(default_logger),logger)
    priority = Option.default({Info},priority)
    color = Option.default({black},color)
    do_log(logger, true, priority, color, s)

  //???
  //let lazy_log ?(logger = !default_logger) ?(priority=Info) ?(color=`black) s =
  //  do_log logger true priority color (Lazy.force s)
  //???

  log_access(logger, priority, color, s) =
    logger = Option.default(ServerReference.get(access_logger),logger)
    priority = Option.default({Info},priority)
    color = Option.default({black},color)
    do_log(logger, false, priority, color, s)

  log_error(logger, priority, color, s) =
    logger = Option.default(ServerReference.get(error_logger),logger)
    priority = Option.default({Info},priority)
    color = Option.default({black},color)
    do do_log(ServerReference.get(default_logger), false, priority, color, s)
    do_log(logger, true, priority, color, s)

  emergency(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Emergency}}, {some={red}}, fmt)

  alert(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Alert}}, {some={red}}, fmt)

  critical(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Critical}}, {some={red}}, fmt)

  error(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Error}}, {some={red}}, fmt)

  warning(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Warning}}, {some={yellow}}, fmt)

  notice(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Notice}}, {some={black}}, fmt)

  info(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Info}}, {some={cyan}}, fmt)

  debug(fmt) =
    log_error({some=ServerReference.get(error_logger)}, {some={Debug}}, {some={blue}}, fmt)


  /* ------------------------------------------------------------ */
  /* Command-line configuration                                   */
  /* ------------------------------------------------------------ */

  default_log_opts = {
    verbose=#<Ifstatic:SHOW_LOGS> 8 #<Else> 6 #<End>;
    display=false;
    path=%%BslLogger.get_cwd%%();
    date=false;
    rotate={none};
    rotate_interval={none};
  }

  initialize() = /* Initial parse */
    args = {
      title = "Logging options"
      init = default_log_opts
      anonymous = []
      parsers = [
        CommandLine.int(["--verbose"],"Set the verbosity level (0-8, default {default_log_opts.verbose})","<level>")(
                         (verbose, p ->
                            if verbose < 0 || verbose > 8
                            then do Log.fatal("Logger options","Bad level: {verbose}") p
                            else {p with ~verbose})),
        CommandLine.switch(["--display-logs"],"Display a part (default, access and error) of the server logs directly on the terminal instead of outputting them into log files")(p -> {p with display=true}),
        CommandLine.string(["--logs-path"],"Set the path for storing logs","<string>")(
                            (path, p -> do ServerReference.set(logs_path, {some="{path}/"}) {p with ~path})),
        CommandLine.switch(["--date-logs"],"(simple alternative to rotate-logs) Use the date of the system to name the logs at every launch")(p -> {p with date=true}),
        CommandLine.string(["--rotate-logs"],"Use the utility \"Apache rotatelogs\" at location <path> to log accesses and errors: a new log file will be created every 1 day by default","<path>")(
                            (path, p -> {p with rotate={some=path}})),
        CommandLine.int(["--rotate-interval"],"Rotate logs will be rotated every <int> days","<int>")(
                         (ri, p -> {p with rotate_interval={some=ri}})),
      ]
    }
    log_opts = CommandLine.filter(args)
    //do jlog("Logger.initialize: log_opts={log_opts}")

    do if Option.is_some(log_opts.rotate) && %%BslLogger.os_type%%() == "Unix"
    then
      rotate = Option.get(log_opts.rotate)
      if %%BslFile.exists%%(rotate)
      then
        ServerReference.set(with_rotatelogs, {some=(rotate, Option.default(1,log_opts.rotate_interval))})
      else
        do log({none},{some={Warning}},{some={yellow}},"{rotate} NOT FOUND: logs will just be dated.")
        ServerReference.set(date_logs,true)
    do ServerReference.set(default_logger,{ServerReference.get(default_logger) with filter=log_opts.verbose-1})
    /* Display logs overrides default, access and error logger */
    if log_opts.display
    then
      log_out = make_logger([("STDOUT", {Channel=%%BslLogger.get_stdout%%()})],default_filer_level)
      log_err = make_logger([("STDERR", {Channel=%%BslLogger.get_stderr%%()})],default_filer_level)
      do set_default_logger(log_out)
      do set_access_logger(log_out)
      set_error_logger(log_err)

}}

_ = Logger.initialize() // Should we do this somewhere else???


