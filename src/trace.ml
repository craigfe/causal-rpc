(* Command line interface *)
open Cmdliner

let msg =
  let doc = "The name of the server." in
  Arg.(value & opt (some string) None & info ["n"; "name"] ~docv:"name" ~doc)

let port =
  let doc = "The port on which to run the server." in
  Arg.(value & opt int 8080 & info ["p"; "port"] ~docv:"port" ~doc)

let trace_t = Term.(const Server.main $ msg $ port)

let info =
  let doc = "start a TraceRPC server" in
  Term.info "trace" ~version:"v0.0.0" ~doc

let () = Term.exit @@ Term.eval (trace_t, info)