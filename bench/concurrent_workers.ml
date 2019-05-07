open Lwt.Infix
open Trace_rpc_unix.Intmap (* Use the unix intmap, since it has non-blocking sleep *)

module GitBackend = Irmin_unix.Git.Mem.G
module I = IntPair (Trace_rpc_unix.Make)(GitBackend)
open I

let inc = O.apply increment_op
let sleep = O.apply sleep_op

let worker_pool ?batch_size ?two_phase switch n dir =
  let rec inner n dir =
    match n with
    | 0 -> []
    | n -> let w =
             IntWorker.run
               ~switch
               ~config:(Trace_rpc.Worker.Config.make
                          ~random_selection:true
                          ?batch_size
                          ?two_phase
                          ~name:("worker_" ^ (string_of_int n))
                          ~poll_freq:0.1 ())
               ~dir:("/tmp/irmin/bench/concurrent_workers/worker/" ^ dir ^ "/worker_" ^ (string_of_int n))
               ~client:("file:///tmp/irmin/bench/concurrent_workers/" ^ dir)
               ()
      in w :: inner (n-1) dir
  in inner n dir

let batch () =
  let () = print_endline "Starting batch execution" in
  let root = "/tmp/irmin/bench/concurrent_workers/" in
  let s = Lwt_switch.create () in
  let values = Helpers.sequence_list 1 100 in
  let keys = List.map Helpers.key_from_int values in

  IntMap.empty ~directory:(root ^ "small_map") ()
  >>= IntMap.add_all (Helpers.zip keys (List.map Int64.of_int values))
  >>= fun m -> Lwt.pick @@ (worker_pool s 4 "small_map") @ [
      let rec inner n = match n with
        | 10 -> Lwt.return_unit
        | n ->
          Lwt.return @@ Core.Time_ns.now ()
          >>= fun init -> IntMap.map ~timeout:10.0 (O.apply sleep_op 0.01) m
          >>= fun _result ->

          let final = Core.Time_ns.now () in
          let span = Core.Time_ns.abs_diff init final in
          print_string @@ Fmt.strf "%a,%a\n" Core.Time_ns.pp final Core.Time_ns.Span.pp span;
          inner (n + 1)

      in inner 1
    ]
  >|= fun _ignored -> ()

let () =
  Random.self_init ();

  (* Configure tracing *)
  (* let buffer = MProf_unix.mmap_buffer ~size:1000000 "trace.ctf" in
   * let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
   * MProf.Trace.Control.start trace_config; *)

  (* Configure logging *)
  (* Trace_rpc.Helpers.set_reporter ();
   * Logs.set_level (Some Logs.Info); *)

  Lwt_main.run (batch ())

