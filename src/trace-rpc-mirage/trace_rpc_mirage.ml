module Make (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S): Trace_rpc.Backend.S
  with type Store.contents = Contents.t
   and type Store.branch = string
   and type Store.step = string
   and type Store.key = Irmin.Path.String_list.t
= struct
  module Store =  Irmin_mirage.Git.KV (GitImpl) (Contents)

  let make_info = Irmin_unix.info
  let remote_of_uri x = Store.remote x
  let sleep = OS.Time.sleep
  let yield = Lwt_main.yield
  let initialise = Irmin_unix.set_listen_dir_hook
end
