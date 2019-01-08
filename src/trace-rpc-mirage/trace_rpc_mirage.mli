open Mirage_types_lwt

module Make (T: TIME): Trace_rpc.Backend.MAKER
