
(* module IntPair = Trace_rpc.Intmap.IntPair (Make) (Irmin_mirage.Git.FS.G) *)

(* let implementation style_renderer level ?name ?dir client =
 *   Fmt_tty.setup_std_outputs ?style_renderer ();
 *   Logs.set_level level;
 *   Logs.set_reporter (Logs_fmt.reporter ());
 *   Main.run ?name ?dir client
 * 
 * (\* Command line interface *\)
 * open Cmdliner
 * 
 * let name =
 *   let doc = "The name of the worker." in
 *   Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"name" ~doc)
 * 
 * let dir =
 *   let doc = "The directory in which to clone the local repository." in
 *   Arg.(value & opt (some dir) None & info ["d"; "dir"] ~docv:"directory" ~doc)
 * 
 * let client = Arg.(value & (pos_all string []) & info [] ~docv:"client")
 * 
 * let worker_t =
 *   let env = Arg.env_var "TRACE_VERBOSITY" in
 *   Term.(const implementation $ Fmt_cli.style_renderer () $ Logs_cli.level ~env () $ name $ dir $ client)
 * 
 * let worker_info = Term.info "trace_worker" ~version:"v0.0.0" ~doc:"start a TraceRPC worker"
 * 
 * let run () =
 *   match Term.(eval (worker_t, worker_info)) with
 *   | `Error _ -> exit 1
 *   | _ -> if Logs.err_count () > 0 then exit 1 else () *)
