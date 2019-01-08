open Mirage_types_lwt

module Make (T: TIME) (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S): Trace_rpc.Backend.S
  with type Store.contents = Contents.t
   and type Store.branch = string
   and type Store.step = string
   and type Store.key = Irmin.Path.String_list.t
= struct
  module Store = Irmin_mirage.Git.KV (GitImpl) (Contents)
  module Info = Irmin_mirage.Info

  let make_info ?author fmt =
    Fmt.kstrf (fun msg () ->
        let date = Int64.zero in
        let author = match author with
          | Some a -> a
          | None -> "Irmin-default"
        in
        Irmin.Info.v ~date ~author msg
      ) fmt

  let remote_of_uri x = Store.remote x
  let sleep f = Duration.of_f f |> T.sleep_ns
  let yield () = Lwt.return_unit (* Lwt_main.yield *)
  let initialise = (fun () -> ()) (* Irmin_unix.set_listen_dir_hook *)
end
