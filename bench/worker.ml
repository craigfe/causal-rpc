open Cmdliner
open Intmap

let log_source =
  let doc = "Log source" in
  Arg.(value & flag & info ["log-source"] ~doc)

let random_selection =
  let doc = "Random selection algorithm" in
  Arg.(value & flag & info ["random"] ~doc)

let batch_size =
  let doc = "Batch size" in
  Arg.(value & opt int 1 & info ["batch-size"] ~doc)

let thread_count =
  let doc = "Thread count" in
  Arg.(value & opt int 1 & info ["thread-count"] ~doc)

let worker_name =
  let doc = "Name" in
  Arg.(value & opt string "worker-default" & info ["name"] ~doc)

let poll_freq =
  let doc = "Polling frequency" in
  Arg.(value & opt float 1.0 & info ["poll-freq"] ~doc)

let two_phase =
  let doc = "Two-phase selection" in
  Arg.(value & flag & info ["two-phase"] ~doc)

let dir =
  let doc = "Directory" in
  let docv = "DIR" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc ~docv)

let client =
  let doc = "Client" in
  let docv = "CLIENT" in
  Arg.(required & pos ~rev:true 1 (some string) None & info [] ~doc ~docv)

let worker log_source random_selection batch_size thread_count name
    poll_freq two_phase dir client =

  Trace_rpc.Helpers.set_reporter ();
  Logs.set_level (Some Logs.Debug);

  let config = Trace_rpc.Worker.Config.make
      ~log_source
      ~random_selection
      ~batch_size
      ~thread_count
      ~name
      ~poll_freq
      ~two_phase
      ()
  in
  IntWorker.run ~config ~dir ~client ()

let cmd =
  Term.(const worker $ log_source $ random_selection $ batch_size $ thread_count $ worker_name $ poll_freq $ two_phase $ dir $ client), Term.info "worker" ~version:"v1.0.0"

let config =
  Term.(exit @@ eval cmd)
