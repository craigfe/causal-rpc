module Make (GitImpl: Irmin_git.G) (Contents: Irmin.Contents.S): Trace_rpc.Store.S
  with type contents = Contents.t
   and type branch = string
   and type step = string
   and type key = Irmin.Path.String_list.t
= struct
  include Irmin_unix.Git.KV (GitImpl) (Contents)

  module Info = struct
    let make = Irmin_unix.info
  end

  let remote_of_uri x = remote x
  let sleep = Lwt_unix.sleep
  let yield = Lwt_main.yield
  let initialise = Irmin_unix.set_listen_dir_hook
end
