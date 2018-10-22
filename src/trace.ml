let implementation style_renderer level message port =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());

  Server.main message port

(* Command line interface *)
open Cmdliner

let msg =
  let doc = "The name of the server." in
  Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"name" ~doc)

let port =
  let doc = "The port on which to run the server." in
  Arg.(value & opt int 8080 & info ["p"; "port"] ~docv:"port" ~doc)

let trace_t =
  let env = Arg.env_var "TOOL_VERBOSITY" in
  Term.(const implementation $ Fmt_cli.style_renderer () $ Logs_cli.level ~env () $ msg $ port)

let trace_info = Term.info "trace" ~version:"v0.0.0" ~doc:"start a TraceRPC server"

let main () =
  match Term.(eval (trace_t, trace_info)) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)

let () = main ()
