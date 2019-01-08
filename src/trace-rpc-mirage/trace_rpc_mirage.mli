open Mirage_types_lwt

module Make
    (T: TIME)
    (Context: sig val r: Resolver_lwt.t val c: Conduit_mirage.t end): Trace_rpc.Backend.MAKER
