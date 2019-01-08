open Mirage_types_lwt

module Make
    (T: TIME)
    (P: PCLOCK)
    (Context: sig
       val r: Resolver_lwt.t
       val c: Conduit_mirage.t
       val p: P.t
     end): Trace_rpc.Backend.MAKER
