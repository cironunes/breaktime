open Cmdliner;

let setup_verbosity = (level, debug) => {
  Fmt_tty.setup_std_outputs();
  if (debug) {
    Logs.set_level(Some(Logs.Debug));
  } else {
    Logs.set_level(level);
  };
  Logs.set_reporter(Logs_fmt.reporter());
};
module Args = {
  let debug = {
    let doc = "Shortcut for debugging verbosity";
    Arg.(value & flag & info(["d", "debug"], ~doc));
  };

  let verbosity = Term.(const(setup_verbosity) $ Logs_cli.level() $ debug);

  let break = {
    let doc = "How much time for break? (mins)";
    Arg.(
      value
      & opt(some(float), None)
      & info(["b", "break"], ~docv="BREAK", ~doc)
    );
  };
  let work = {
    let doc = "How much work before a break? (mins)";
    Arg.(
      value
      & opt(some(float), None)
      & info(["w", "work"], ~docv="WORK", ~doc)
    );
  };

  let options = {
    let doc = "What to do on breaks?";
    Arg.(
      value
      & opt(some(list(string)), None)
      & info(["o", "options"], ~docv="OPTIONS", ~doc)
    );
  };
};

let rec brk = (endTime) => {
  open Core.Time;
  let timeNow = now();
  // Console.debug("Time Now: " ++ (timeNow |> to_string));
  // Console.debug("Time End: " ++ (endTime |> to_string));
  let result = is_earlier(timeNow, ~than=endTime);

  if (result) {
    let%lwt () = Lwt_unix.sleep(1.0);
    brk(endTime);
  } else {
    Console.log("It's work time again!");
    Lwt.return_unit;
  }
};

let randomElement = lst => {
  let n = Random.int(List.length(lst));
  List.nth(lst, n);
};

let cmd = (_flags, work: option(float), break, options) => {
  open Core.Time;
  let startTime = now();
  let workTimeInMinutes = Option.get(work);
  let breakTimeInMinutes = Option.get(break);
  let optionList = Option.get(options);
  let endTime = add(startTime, Span.of_min(workTimeInMinutes));
  let breakEndTime = add(endTime, Span.of_min(breakTimeInMinutes));

  Console.log(Lib.Util.logKeyValue("Work", workTimeInMinutes |> string_of_float));
  Console.log(Lib.Util.logKeyValue("Break", breakTimeInMinutes |> string_of_float));
  Console.log(Lib.Util.logKeyValue("Options", optionList |> String.concat(",")));
  Console.log(Lib.Util.logKeyValue("Start Time", startTime |> to_string));
  Console.log(Lib.Util.logKeyValue("End Time", endTime |> to_string));

  let rec tic = (endTime, workTimeInMinutes) => {
    open Core.Time;
    let timeNow = now();
    // Console.debug("Time Now: " ++ (timeNow |> to_string));
    // Console.debug("Time End: " ++ (endTime |> to_string));
    let result = is_earlier(timeNow, ~than=endTime);

    if (result) {
      let%lwt () = Lwt_unix.sleep(1.0);
      tic(endTime, workTimeInMinutes);
    } else {
      Console.log("It's break time!");
      Console.log("Why don't you go for " ++ randomElement(optionList))
      // TODO: select one option
      brk(breakEndTime);
      // Lwt.return_unit;
    }
  };

  Lwt_main.run(tic(endTime, workTimeInMinutes)) |> ignore;
};

let cmd_t =
  Term.(
    const(cmd) $ Args.verbosity $ Args.work $ Args.break $ Args.options
  );

Term.eval((cmd_t, Term.info("breaktime"))) |> Term.exit;
