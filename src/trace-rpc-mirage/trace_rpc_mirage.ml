open Mirage_types_lwt

module Make
    (T: TIME)
    (P: PCLOCK)
    (Context: sig
       val r: Resolver_lwt.t
       val c: Conduit_mirage.t
       val p: P.t
     end)
    (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S): Trace_rpc.Backend.S
  with type Store.contents = Contents.t
   and type Store.branch = string
   and type Store.step = string
   and type Store.key = Irmin.Path.String_list.t
= struct
  module Store = Irmin_mirage.Git.KV (GitImpl) (Contents)

  let make_info ?(author="tracerpc-default") =
    let module I = Irmin_mirage.Info (struct let name = author end) (P) in
    I.f Context.p

  let remote_of_uri x = Store.remote ~conduit:Context.c ~resolver:Context.r x
  let sleep f = Duration.of_f f |> T.sleep_ns
  let yield () = T.sleep_ns Int64.zero
  let initialise = (fun () -> ())
end
